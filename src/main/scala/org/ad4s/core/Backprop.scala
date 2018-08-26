package org.ad4s.core

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

trait Times[A] {
  def times(a1:A, a2: A):A
}