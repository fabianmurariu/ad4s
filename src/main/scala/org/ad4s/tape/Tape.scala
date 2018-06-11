package org.ad4s.tape

import cats.Traverse
import cats.data.State

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.ClassTag

// port from https://github.com/Rufflewind/revad/blob/master/src/tape.rs
// build the intuition
class Tape[A: NumKernel] {
  val nodes: ArrayBuffer[Node[A]] = ArrayBuffer.empty[Node[A]]

  def len: Int = nodes.length

  def push2[T](dep0: Int = len, weight0: A = implicitly[NumKernel[A]].zero,
               dep1: Int = len, weight1: A = implicitly[NumKernel[A]].zero): Int = {
    val n = len
    nodes += Node((dep0, dep1), (weight0, weight1))
    n
  }

  def _var(v: A): Var[A] = Var(v, push2())

  def _varS(v: A): State[Tape[A], Var[A]] =
    State.pure(Var(v, push2()))
}

object Tape {
  def empty[T: NumKernel] = new Tape[T]()
}

case class Node[T](deps: (Int, Int), vals: (T, T))

case class Var[T](v: T, idx: Int)

case class Grad[T](derivs: Vector[T])

object Var {
  // aka Lazy var
  type LVar[T] = State[Tape[T], Var[T]]

  implicit def liftVar[T](a: Var[T]): LVar[T] = State.pure[Tape[T], Var[T]](a)

  implicit class VarOps[T](val a: LVar[T]) extends AnyVal {
    def +(b: LVar[T])(implicit N: NumKernel[T]): LVar[T] =
      for {
        va <- a
        vb <- b
        plus_ab <- State[Tape[T], Var[T]] {
          t =>
            t -> Var(N.plus(va.v, vb.v), t.push2(va.idx, N.one, vb.idx, N.one))
        }
      } yield plus_ab

    def *(b: LVar[T])(implicit N: NumKernel[T]): LVar[T] =
      for {
        va <- a
        vb <- b
        times_ab <- State[Tape[T], Var[T]] {
          t =>
            t -> Var(N.times(va.v, vb.v), t.push2(va.idx, vb.v, vb.idx, va.v))
        }
      } yield times_ab

    def **(b: LVar[T])(implicit N: NumKernel[T]): LVar[T] =
      for {
        va <- a
        vb <- b
        pow_ab <- State[Tape[T], Var[T]] {
          t =>
            val dx = N.times(vb.v, N.pow(va.v, N.minus(vb.v, N.one)))
            val dy = N.times(N.pow(va.v, vb.v), N.log(va.v))
            t -> Var(N.pow(va.v, vb.v), t.push2(va.idx, dx, vb.idx, dy))
        }
      } yield pow_ab

    def eval(implicit N: NumKernel[T], CT: ClassTag[T]): State[Tape[T], (Var[T], Grad[T])] =
      for {
        va <- a
        g <- State.inspect {
          s: Tape[T] =>
            val derivs = Array.fill(s.len) {
              N.zero
            }(CT)
            derivs(va.idx) = N.one
            for (i <- (s.len - 1) to 0 by -1) {
              val n = s.nodes(i)
              val deriv = derivs(i)
              val (k, j) = n.deps // left and right dependency of this node
              val (wk, wj) = n.vals // left and right weights of this node
              derivs(k) = N.plus(derivs(k), N.times(wk, deriv))
              derivs(j) = N.plus(derivs(j), N.times(wj, deriv))
            }
            va -> Grad[T](derivs.toVector)
        }
      } yield g

  }

  def reduce[F[_], T: NumKernel](vs: F[LVar[T]])(f: (LVar[T], LVar[T]) => LVar[T])
                                (implicit R: Traverse[F]): Option[LVar[T]] =
    R.reduceLeftOption(vs)(f)

  def reduceSum[F[_], T: NumKernel](vs: F[LVar[T]])(implicit R: Traverse[F]): Option[LVar[T]] =
    reduce(vs)(_ + _)

  def sin[T](a: LVar[T])(implicit N: NumKernel[T]): LVar[T] = for {
    va <- a
    sin_a <- State[Tape[T], Var[T]] {
      t =>
        t -> Var(N.sin(va.v), t.push2(va.idx, N.cos(va.v)))
    }
  } yield sin_a

}

//import shapeless._
//import UnaryTCConstraint._
//
//case class TapeNode[N <: HList *->* InpRef, A](tnInputs: N, tnGrad: A => N)