package org.ad4s.core.backprop

trait Backprop[A] extends Zeros[A] with Ones[A] with Sum[A]

trait Zeros[A] {
  def zeros(a: A): A
}

trait Ones[A] {
  def ones(a: A): A
}

trait Sum[A] {
  def add(a1: A, a2: A): A
}

object Backprop {

  object implicits {

    import spire.implicits._
    import spire.math.Fractional

    implicit def backpropFromFractional[T: Fractional]: Backprop[T] = new Backprop[T] {
      override def zeros(a: T): T = 0

      override def add(a1: T, a2: T): T = a1 + a2

      override def ones(a: T): T = 1
    }
  }
}