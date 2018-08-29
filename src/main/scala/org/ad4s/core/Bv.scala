package org.ad4s.core

import cats.effect.IO
import org.ad4s.core.tape._

case class Bv[T](i: InpRef[T], v: T)

object Bv {

  def apply[T](v: T)(implicit BC: BackpropContext[T], B: Backprop[T]): Bv[T] = {
    val node = Node(
      inputs = Seq.empty[InpRef[T]],
      zero = B.zeros(v),
      grad = { _: T => Seq.empty[T] }
    )
    val idx = BC.insertNode(node).unsafeRunSync()
    new Bv(InpRef(idx, B.add), v)
  }

  object Implicits {

    implicit class BvOps[T](val a: Bv[T]) extends AnyVal {
      def +(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        plus(a, b).unsafeRunSync()

      def *(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        times(a, b).unsafeRunSync()
    }

    implicit def kernelFromNumeric[T](implicit N: Fractional[T]): Kernel[T] = new Kernel[T] {
      override def times(a: T, b: T): T = N.times(a, b)

      override def plus(a: T, b: T): T = N.plus(a, b)

      override def minus(a: T, b: T): T = N.minus(a, b)

      override def div(a: T, b: T): T = N.div(a, b)

      override def one: T = N.one

      override def zero(t: T): T = N.zero

      override def negate(t: T): T = N.negate(t)
    }

    implicit val mathsForDouble: Maths[Double] = new Maths[Double] {
      override def sin(x: Double): Double = Math.sin(x)

      override def cos(x: Double): Double = Math.cos(x)

      override def exp(x: Double): Double = Math.exp(x)

      override def log(x: Double): Double = Math.log(x)

      override def pow(x: Double, n: Double): Double = Math.pow(x, n)
    }
  }

  object Ops {
    private def sin_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
      B.insertNode(Node[T](
        inputs = Seq(x.i),
        zero = K.zero(x.v),
        grad = { g: T => Seq(g * M.cos(x.v)) })
      ).map(i => Bv(InpRef(i, K.plus), M.sin(x.v)))

    private def exp_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
      B.insertNode(Node[T](
        inputs = Seq(x.i),
        zero = K.zero(x.v),
        grad = { g: T => Seq(g * M.exp(x.v)) })
      ).map(i => Bv(InpRef(i, K.plus), M.exp(x.v)))

    private def log_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
      B.insertNode(Node[T](
        inputs = Seq(x.i),
        zero = K.zero(x.v),
        grad = { g: T => Seq(g / x.v) })
      ).map(i => Bv(InpRef(i, K.plus), M.log(x.v)))

    def sin[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
      sin_(x).unsafeRunSync()

    def exp[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
      exp_(x).unsafeRunSync()

    def log[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
      log_(x).unsafeRunSync()

  }

  def plus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v + b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(K.one * g, K.one * g) }))
    } yield Bv(InpRef(i, K.plus), v)

  def times[T](a: Bv[T], b: Bv[T])
              (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v * b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(b.v * g, a.v * g) }))
    } yield Bv(InpRef(i, K.plus), v)
}