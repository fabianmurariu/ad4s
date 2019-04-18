package org.ad4s.core.tape

import cats.{Eval, Foldable, Monad}
import cats.effect.IO
import cats.effect.concurrent
import org.ad4s.core.backprop._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[tape] class Tape {
  val nodes2: ArrayBuffer[TapeNode[_ <: Any]] = ArrayBuffer.empty[TapeNode[_ <: Any]]

  def len: Int = nodes2.length

  def reverse(n: Int = this.len): IndexedSeq[Int] = (n to 0 by -1)

  private[tape] def insert0(tn: TapeNode[_ <: Any]): IO[Int] = IO {
    val n = len
    nodes2 += tn
    n
  }

}

object Tape {

  implicit def indexedSeqIsFoldable[T]: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldLeft[A, B](fa: IndexedSeq[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: IndexedSeq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(i: Int): Eval[B] =
        if (i < fa.length) f(fa(i), Eval.defer(loop(i + 1))) else lb

      Eval.defer(loop(0))
    }
  }

  def runGrads[X, Z](m: TapeEvaluatorMagnet[X, Z])
                    (x: X)(implicit B: Backprop[Z]): (Z, X) = {
    implicit val BC: BackpropContext = new BackpropContext(new Tape)

    val (out, gradBuilder) = m.eval(x)
    val grad = evalGrads(out, BC.tape)(B)
    (out.v, gradBuilder(grad))
  }

  def evalGrads2[T](bv: d[T], t: Tape): IO[Grad] = {

    if (bv.i == DConst) IO.pure(Grad(t.nodes2.map(_.zero).toVector))
    else {
      import cats.implicits._
      val DRef(di, _) = bv.i

      for {
        derivsRef <- concurrent.Ref.of[IO, mutable.Seq[Any]](t.nodes2.map(_.zero))
        emptyGrads <- derivsRef.get
        grads <- t.reverse(di).foldM[IO, mutable.Seq[Any]](emptyGrads) {
          (derivs: mutable.Seq[Any], j) =>
            IO {
              val deriv = derivs(j)
              t.nodes2(j) match {
                case _: Node0[Any]@unchecked =>
                  derivs
                case n1: Node1[Any, Any]@unchecked =>
                  val w = n1.grad(deriv)
                  derivs(n1.inp1.i) = n1.inp1.sum.add(derivs(n1.inp1.i), w)
                  derivs
                case n2: Node2[Any, Any, Any]@unchecked =>
                  val (w1, w2) = n2.grad(deriv)
                  derivs(n2.inp1.i) = n2.inp1.sum.add(derivs(n2.inp1.i), w1)
                  derivs(n2.inp2.i) = n2.inp2.sum.add(derivs(n2.inp2.i), w2)
                  derivs
              }
            }
        }
      } yield Grad(grads.toVector)

    }
  }

  def evalGrads[T](bv: d[T], t: Tape)(backprop: Ones[T]): Grad = {
    val derivs = t.nodes2.map(_.zero) // this may be wrong
    if (bv.i == DConst) Grad(derivs.toVector)
    else {
      val DRef(di, _) = bv.i
      derivs(di) = backprop.ones(bv.v)
      for (j <- t.reverse(di)) {
        val deriv = derivs(j)
        t.nodes2(j) match {
          case _: Node0[Any]@unchecked => /*do nothing*/
          case n1: Node1[Any, Any]@unchecked =>
            val w = n1.grad(deriv)
            derivs(n1.inp1.i) = n1.inp1.sum.add(derivs(n1.inp1.i), w)
          case n2: Node2[Any, Any, Any]@unchecked =>
            val (w1, w2) = n2.grad(deriv)
            derivs(n2.inp1.i) = n2.inp1.sum.add(derivs(n2.inp1.i), w1)
            derivs(n2.inp2.i) = n2.inp2.sum.add(derivs(n2.inp2.i), w2)
        }
      }
      Grad(derivs.toVector)
    }
  }
}

case class InpRef[T](i: Int, sum: Sum[T])

/* sum type might need extension */

sealed trait TapeNode[Z] {
  def zero: Z
}

case class Node0[Z](zero: Z) extends TapeNode[Z]

case class Node1[A1, Z](inp1: InpRef[A1], zero: Z, grad: Z => A1) extends TapeNode[Z]

case class Node2[A1, A2, Z](inp1: InpRef[A1], inp2: InpRef[A2], zero: Z, grad: Z => (A1, A2)) extends TapeNode[Z]

case class Grad(dxs: Vector[Any])

class BackpropContext(private[core] val tape: Tape = new Tape) {
  private[core] def insertNode[T](tn: TapeNode[T]): IO[Int] =
    tape.insert0(tn)
}
