package org.ad4s.core.op

import cats.effect.IO
import org.ad4s.core.backprop.{Bv, Sum, Zeros}
import org.ad4s.core.tape._

sealed trait Op

trait Op1[A, Z] extends (A => (Z, Z => A))

trait Op2[A, B, Z] extends ((A, B) => (Z, Z => (A, B)))

object Op {

  def liftOp1[A1, Z](a: Bv[A1])(op: Op1[A1, Z])
                    (implicit B: BackpropContext, Z: Zeros[Z], Sum: Sum[A1]): IO[Bv[Z]] =
    for {
      c <- IO.pure(op(a.v))
      (z, gradFn) = c
      i <- B.insertNode(Node1[A1, Z](
        inp1 = InpRef(a.i, Sum),
        zero = Z.zeros(z),
        grad = { g => gradFn(g) }))
    } yield Bv(i, z)

  @inline
  def liftOp2[A1, A2, Z](a: Bv[A1], b: Bv[A2])(op: Op2[A1, A2, Z])
                       (implicit B: BackpropContext, Z: Zeros[Z], Sum1:Sum[A1], Sum2:Sum[A2]): IO[Bv[Z]] =
    for {
      c <- IO.pure(op(a.v, b.v))
      (z, gradFn) = c
      i <- B.insertNode(Node2[A1, A2, Z](
        inp1 = InpRef(a.i, Sum1),
        inp2 = InpRef(b.i, Sum2),
        zero = Z.zeros(z),
        grad = { g => gradFn(g) }
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

  trait Det[A, Z] extends Op1[A, Z]

}
