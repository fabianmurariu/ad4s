package org.ad4s.tape

trait NumKernel[T] {
  def zero: T

  def one: T

  def plus(a: T, b: T): T

  def times(a: T, b: T): T

  def sin(t: T): T

  def cos(t: T): T
}

object NumKernel{
  implicit val doubleIsNumKernel:NumKernel[Double] = new NumKernel[Double] {
    override def sin(t: Double): Double = Math.sin(t)

    override def cos(t: Double): Double = Math.cos(t)

    override def zero: Double = 0d

    override def one: Double = 1d

    override def plus(a: Double, b: Double): Double = a + b

    override def times(a: Double, b: Double): Double = a * b
  }
}
