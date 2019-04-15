package org.ad4s.core.backprop.laws

import org.ad4s.core.backprop.Backprop
import cats.laws._

trait BackpropLaws[A] {
  implicit def B: Backprop[A]

  def identityLeft(x: A, y: A): IsEq[A] =
    B.add(x, B.zeros(y)) <-> x

  def identityRight(x: A, y: A): IsEq[A] =
    B.add(B.zeros(x), y) <-> y

  def commutativity(x: A, y: A): IsEq[A] =
    B.add(x, y) <-> B.add(y, x)

  def associativity(x:A, y:A, z:A):IsEq[A] =
    B.add(x, B.add(y, z)) <-> B.add(B.add(x, y), z)

  def idempotence(x:A) =
    (B.zeros _ andThen B.zeros).apply(x) <-> B.zeros(x)
}

object BackpropLaws {
  def apply[A](implicit ev: Backprop[A]): BackpropLaws[A] =
    new BackpropLaws[A] {
      override implicit def B: Backprop[A] = ev
    }
}
