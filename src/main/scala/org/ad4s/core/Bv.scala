package org.ad4s.core

import cats.effect.IO
import org.ad4s.core.tape._

case class Bv[T](i: InpRef, v: T)

object Bv {

  object Implicits {

    implicit class BvOps[T](val a: Bv[T]) extends AnyVal {
      def +(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        plus(a, b).unsafeRunSync()

      def *(b:Bv[T])(implicit B:BackpropContext[T], K:Kernel[T]):Bv[T] =
        times(a, b).unsafeRunSync()
    }

    implicit def kernelFromNumeric[T](implicit N:Fractional[T]):Kernel[T] = new Kernel[T]{
      override def times(a: T, b: T): T = N.times(a, b)

      override def plus(a: T, b: T): T = N.plus(a, b)

      override def minus(a: T, b: T): T = N.minus(a, b)

      override def one: T = N.one

      override def zero(t: T): T = N.zero
    }
  }
  def plus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v + b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(K.one * g, K.one * g)}))
    } yield Bv(InpRefN(i, K.plus), v)

  def times[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v * b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(b.v * g, a.v * g)}))
    } yield Bv(InpRefN(i, K.plus), v)
}