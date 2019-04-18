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

  import spire.implicits._
  object implicits extends NumericInstances with FractionalInstances

  trait NumericInstances {

    implicit def backpropFromNumeric[T: spire.math.Numeric]: Backprop[T] = new Backprop[T] {
      override def add(a1: T, a2: T): T = a1 + a2

      override def ones(a: T): T = 1

      override def zeros(a: T): T = 0
    }
  }

  trait FractionalInstances {
    implicit def backpropFromFractional[T: spire.math.Fractional]: Backprop[T] = new Backprop[T] {
      override def zeros(a: T): T = 0

      override def add(a1: T, a2: T): T = a1 + a2

      override def ones(a: T): T = 1
    }

  }

}