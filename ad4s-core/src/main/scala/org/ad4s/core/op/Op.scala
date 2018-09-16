package org.ad4s.core.op

import cats.effect.IO
import org.ad4s.core.backprop.{Backprop, Bv, Sum, Zeros}
import org.ad4s.core.tape.{BackpropContext, InpRef, Node}

sealed trait Op

trait Op1[A, Z] extends (A => (Z, Z => A))

trait Op2[A, B, Z] extends ((A, B) => (Z, Z => (A, B)))

object Op {

  def liftOp1[A](a: Bv[A])(op: Op1[A, A])
                (implicit B: BackpropContext, Z: Zeros[A], Sum: Sum[A]): IO[Bv[A]] =
    for {
      c <- IO.pure(op(a.v))
      (z, gradFn) = c
      i <- B.insertNode(Node[A](
        inputs = Seq(InpRef(a.i, Sum)),
        zero = Z.zeros(z),
        grad = {
          g =>
            Seq(gradFn(g))
        }
      ))
    } yield Bv(i, z)

  @inline
  def liftOp2[A](a: Bv[A], b: Bv[A])(op: Op2[A, A, A])
                (implicit B: BackpropContext, Bp: Backprop[A]): IO[Bv[A]] =
    for {
      c <- IO.pure(op(a.v, b.v))
      (z, gradFn) = c
      i <- B.insertNode(Node[A](
        inputs = Seq(InpRef(a.i, Bp), InpRef(b.i, Bp)),
        zero = Bp.zeros(z),
        grad = {
          g =>
            val (da, db) = gradFn(g)
            Seq(da, db)
        }
      ))
    } yield Bv(i, z)
}

object Ops {

  trait Plus[A, B, Z] extends Op2[A, B, Z]

  trait Minus[A, B, Z] extends Op2[A, B, Z]

  trait Times[A, B, Z] extends Op2[A, B, Z]

  trait Div[A, B, Z] extends Op2[A, B, Z]

  trait Sin[A, Z] extends Op1[A, Z]

  trait Cos[A, Z] extends Op1[A, Z]

  trait Sigmoid[A, Z] extends Op1[A, Z]

  trait Relu[A, Z] extends Op1[A, Z]

  trait Softmax[A, Z] extends Op2[A, Int, Z] // wants dimension for softmax

  trait Log[A, Z] extends Op1[A, Z]

  trait Exp[A, Z] extends Op1[A, Z]

  trait Pow[A, B, Z] extends Op2[A, B, Z]

}
