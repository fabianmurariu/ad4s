package org.ad4s.core

import org.ad4s.core.tape._

case class Bv[T](i: Int, v: T)

object Bv {

  def apply[T](v: T)(implicit BC: BackpropContext[T], B: Backprop[T]): Bv[T] = {
    val node = Node(
      inputs = Seq.empty[InpRef[T]],
      zero = B.zeros(v),
      grad = { _: T => Seq.empty[T] }
    )
    val idx = BC.insertNode(node).unsafeRunSync()
    new Bv(idx, v)
  }

  object Implicits {

    implicit class BvOps[T](val a: Bv[T]) extends AnyVal {
      def +(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        Ops.plus(a, b).unsafeRunSync()

      def -(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        Ops.minus(a, b).unsafeRunSync()

      def *(b: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T]): Bv[T] =
        Ops.times(a, b).unsafeRunSync()
    }

    implicit def kernelFromNumeric[T](implicit N: Fractional[T]): Kernel[T] = new Kernel[T] {
      override def times(a: T, b: T): T = N.times(a, b)

      override def plus(a: T, b: T): T = N.plus(a, b)

      override def minus(a: T, b: T): T = N.minus(a, b)

      override def div(a: T, b: T): T = N.div(a, b)

      override def one: T = N.one

      override def zero(t: T): T = N.zero

      override def negate(t: T): T = N.negate(t)

      override def fromInt(i: Int): T = N.fromInt(i)
    }

    implicit val mathsForDouble: Maths[Double] = new Maths[Double] {
      override def sin(x: Double): Double = Math.sin(x)

      override def cos(x: Double): Double = Math.cos(x)

      override def exp(x: Double): Double = Math.exp(x)

      override def log(x: Double): Double = Math.log(x)

      override def pow(x: Double, n: Double): Double = Math.pow(x, n)
    }
  }

}