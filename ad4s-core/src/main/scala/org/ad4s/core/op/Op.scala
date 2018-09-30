package org.ad4s.core.op

import cats.effect.IO
import org.ad4s.core.backprop._
import org.ad4s.core.tape._

sealed trait Op

trait Op1[A, Z] extends (A => (Z, Z => A))

trait Op2[A, B, Z] extends ((A, B) => (Z, Z => (A, B)))

object Op {

  def liftOp1[A1, Z](a: d[A1])(op: Op1[A1, Z])
                    (implicit Z: Zeros[Z], Sum: Sum[A1]): IO[d[Z]] = a.i match {
    case DConst => IO.pure(op(a.v)._1).map(d(DConst, _))
    case dr@DRef(i, bc) =>
      for {
        c <- IO.pure(op(a.v))
        (z, gradFn) = c
        i <- bc.insertNode(Node1[A1, Z](
          inp1 = InpRef(i, Sum),
          zero = Z.zeros(z),
          grad = { g => gradFn(g) }))
      } yield d(dr.copy(i = i), z)
  }

  @inline
  def liftOp2[A1, A2, Z](a: d[A1], b: d[A2])(op: Op2[A1, A2, Z])
                        (implicit Z: Zeros[Z], Sum1:Sum[A1], Sum2:Sum[A2]): IO[d[Z]] =
    (a.i, b.i) match {
      case (DConst, DConst) => IO.pure(op(a.v, b.v)._1).map(d(DConst, _))
      case (DConst, DRef(i, bc)) =>
        for {
          c <- IO.pure(op(a.v, b.v))
          (z, gradFn) = c
          k <- bc.insertNode(Node1[A2, Z](
            inp1 = InpRef(i, Sum2),
            zero = Z.zeros(z),
            grad = { g => gradFn(g)._2 }
          ))
        } yield d(DRef(k, bc), z)
      case (DRef(i, bc), DConst) =>
        for {
          c <- IO.pure(op(a.v, b.v))
          (z, gradFn) = c
          k <- bc.insertNode(Node1[A1, Z](
            inp1 = InpRef(i, Sum1),
            zero = Z.zeros(z),
            grad = { g => gradFn(g)._1 }
          ))
        } yield d(DRef(k, bc), z)
      case (DRef(i, bc), DRef(j, _)) =>
        for {
          c <- IO.pure(op(a.v, b.v))
          (z, gradFn) = c
          k <- bc.insertNode(Node2[A1, A2, Z](
            inp1 = InpRef(i, Sum1),
            inp2 = InpRef(j, Sum2),
            zero = Z.zeros(z),
            grad = { g => gradFn(g) }
          ))
        } yield d(DRef(k, bc), z)
    }
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
