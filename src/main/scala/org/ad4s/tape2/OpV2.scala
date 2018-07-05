package org.ad4s.tape2

import cats.Traverse
import cats.effect.IO
import org.ad4s.core.{Backprop, Ones, Sum, Zeros}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

/**
  * Non Type generic take at Op
  *
  * @param run
  * evaluation method
  * @tparam T
  * most likely numeric but could be array, etc..
  */
case class OpV2[T](run: Seq[T] => (T, T => Seq[T]))

case class BVar[T](ref: BRef, v: T)

object BVar {
  implicit def zf[T](implicit N: Numeric[T]): Backprop[T] = new Backprop[T] {
    override def add(a1: T, a2: T): T = N.plus(a1, a2)

    override def ones(a: T): T = N.one

    override def zeros(a: T): T = N.zero
  }
}

sealed trait BRef

case object BRC extends BRef // assume constant

// marker for every op added between inputs
case class BRIx(i: Int) extends BRef

// marker to initialize the inputs to the computation Seq[BVar[T]] => BVar[T]
case class BRInp(i: Int) extends BRef

case class TapeNode[T](inputs: Seq[InpRef[T]], grad: T => Seq[T])

case class SomeTapeNode[T](zero: T, tn: TapeNode[T])

case class InpRef[T](ix: BVar[T], add: (T, T) => T)

case class Tape[T](tape: ArrayBuffer[SomeTapeNode[T]] = ArrayBuffer.empty[SomeTapeNode[T]])

class Runner[T](private[tape2] val rDelta: ArrayBuffer[T], private[tape2] val rInputs: ArrayBuffer[T])

object Runner {
  def apply[T](t: Tape[T], xs: T*)
              (implicit CBF: CanBuildFrom[ArrayBuffer[T], T, ArrayBuffer[T]]): Runner[T] = {
    val deltas = (CBF.apply() ++= t.tape.map(_.zero)).result()
    val inputs = (CBF.apply() ++= xs).result()
    new Runner[T](deltas, inputs)
  }
}

object Backprop {

  def opN[A: Numeric](f: Seq[A] => (A, A => Seq[A])): OpV2[A] =
    OpV2(f)

  def plus[A: Numeric]: OpV2[A] = opN[A] {
    xs =>
      (xs.sum, g => Seq.fill(xs.length)(g))
  }

  def runOp[T: Numeric](op: OpV2[T])(ts: T*): (T, Seq[T]) = {
    val (z, g) = op.run(ts)
    (z, g.apply(implicitly[Numeric[T]].one))
  }

  implicit class TapeOps[T](val w: Tape[T]) extends AnyVal {
  }

  def insertNode0[T](t: Tape[T], tn: TapeNode[T], x: T)
                    (implicit zf: Zeros[T]): IO[BVar[T]] = IO {
    val n = t.tape.length
    val stn = SomeTapeNode(zero = zf.zeros(x), tn)
    t.tape.append(stn)
    BVar(BRIx(n), x)
  }

  def liftOpN_[T](op: OpV2[T])(bvs: Seq[BVar[T]])
                 (implicit W: Tape[T], sumF: Sum[T], zF: Zeros[T]): IO[BVar[T]] = {
    val allConst = bvs.count(_.ref == BRC) == bvs.length
    if (allConst)
      IO.pure(BVar(BRC, op.run(bvs.map(_.v))._1))
    else {
      val (y, g) = op.run(bvs.map(_.v))
      val tn = TapeNode(bvs.map(bv => InpRef(bv, sumF.add)), grad = g)
      insertNode0(W, tn, y)
    }
  }

  def liftOpN[T](op: OpV2[T])(bvs: Seq[BVar[T]])
                (implicit W: Tape[T], sumF: Sum[T], zF: Zeros[T]): BVar[T] =
    liftOpN_(op)(bvs).unsafeRunSync()


  implicit class BVarOps[T](val v: BVar[T]) extends AnyVal {
    def +(u: BVar[T])(implicit N: Numeric[T], W: Tape[T], B: Backprop[T]): BVar[T] = {
      plus2(v, u)
    }
  }

  def plus2[T: Numeric : Tape : Zeros : Sum](v: BVar[T], u: BVar[T]) =
    liftOpN(plus)(Seq(v, u))

  def constBVar[T](t: T) =
    BVar(BRC, t)

  def backprop2[T: Backprop](f: Tape[T] => (BVar[T], BVar[T]) => BVar[T])
                            (x1: T, x2: T): (T, (T, T)) = {
    val (t, Seq(du, dv)) = (backpropN[T] _) {
      tape: Tape[T] =>
        (xs: Seq[BVar[T]]) => {
          val a :: b :: _ = xs
          f(tape)(a, b)
        }
    }.apply(Seq(x1, x2))
    (t, (du, dv))
  }

  /**
    * Given
    *
    * @param f
    * any function with that takes a tape and a bunch of BVars
    * and returns a BVar
    * @param xs
    * and a set of input parameters for them BVars
    * @tparam T
    * all of type T
    * @return
    * a tuple with
    * (the result of the computation, the gradient with respect to the inputs)
    */
  def backpropN[T](f: Tape[T] => Seq[BVar[T]] => BVar[T])
                  (xs: T*)
                  (implicit B: Backprop[T]): (T, Seq[T]) = {
    ???
  }

  def initWengert[T]: IO[Tape[T]] = IO {
    new Tape[T]
  }

  def fillWengert2[T: Backprop](f: Tape[T] => (BVar[T], BVar[T]) => BVar[T])
                               (xs: (T, T)): IO[(Tape[T], T)] =
    (fillWengertN[T] _) {
      tape: Tape[T] =>
        xs: Seq[BVar[T]] => {
          val bva :: bvb :: _ = xs
          f(tape)(bva, bvb)
        }
    }.apply(xs._1 :: xs._2 :: Nil)

  def fillWengertN[T](f: Tape[T] => Seq[BVar[T]] => BVar[T])
                     (xs: T*): IO[(Tape[T], T)] = for {
    w <- initWengert[T]
    o <- IO {
      val go = (x: T, i: Int) => BVar(BRInp(i), x)
      val inpProd: Seq[BVar[T]] = xs.zipWithIndex.map(go.tupled)
      val oVar = f(w)(inpProd)
      oVar.v
    }
  } yield (w, o)

}