package org.ad4s.core.op

import cats.effect.IO
import org.ad4s.core.backprop._
import org.ad4s.core.tape._

sealed trait Op

trait Op1[A, Z] extends (A => (Z, Z => A)) with Op

trait Op2[A, B, Z] extends ((A, B) => (Z, Z => (A, B))) with Op

object Op {

  @inline
  def op1[A, Z](f: A => (Z, Z => A)): Op1[A, Z] = (a: A) => f(a)
  @inline
  def op2[A, B, Z](f: (A, B) => (Z, Z => (A, B))): Op2[A, B, Z] = (a: A, b: B) => f(a, b)

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
                        (implicit Z: Zeros[Z], Sum1: Sum[A1], Sum2: Sum[A2]): IO[d[Z]] =
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
