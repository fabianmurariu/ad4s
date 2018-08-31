package org.ad4s.tape

trait NumKernel[T] {
  def zero: T

  def one: T

  def plus(a: T, b: T): T

  def times(a: T, b: T): T

  def sin(t: T): T

  def cos(t: T): T

  def log(t: T): T

  def pow(a: T, b: T): T

  def neg(a: T): T

  def minus(a: T, b: T): T = plus(a, neg(b))
}

object NumKernel {
  implicit val doubleIsNumKernel: NumKernel[Double] = new NumKernel[Double] {
    override def sin(t: Double): Double = Math.sin(t)

    override def cos(t: Double): Double = Math.cos(t)

    override def zero: Double = 0d

    override def one: Double = 1d

    override def plus(a: Double, b: Double): Double = a + b

    override def times(a: Double, b: Double): Double = a * b

    override def log(t: Double): Double = Math.log(t)

    override def pow(a: Double, b: Double): Double = Math.pow(a, b)

    override def neg(a: Double): Double = -a
  }

  implicit class NumOps[T](val x: T) extends AnyVal {
    def +(y: T)(implicit N: NumKernel[T]): T = N.plus(x, y)
    def *(y: T)(implicit N: NumKernel[T]): T = N.times(x, y)
    def **(y:T)(implicit N: NumKernel[T]): T = N.pow(x, y)
  }

}
