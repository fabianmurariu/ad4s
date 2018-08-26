package org.ad4s.core

import cats.effect.IO
import org.ad4s.core.tape._

case class Bv[T](i: InpRef, v: T)

object Bv {

  implicit class BvOps[T](val a: Bv[T]) extends AnyVal {
    def +(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
      plus(a, b).unsafeRunSync()
  }

  def plus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v + b.v)
      i <- B.insertNode(
        Node2[T](
          inputs = (a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => (K.one * g, K.one * g)}))
    } yield Bv(InpRefN(i, K.plus), v)
}