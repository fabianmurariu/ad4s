package org.ad4s.core

trait Kernel[T] {
  def times(a: T, b: T): T

  def plus(a: T, b: T): T

  def minus(a: T, b: T): T

  def div(a: T, b: T): T

  def one(t: T): T

  def zero(t: T): T

  def negate(t: T): T

  def fromInt(t: T, i: Int): T
}

trait Maths[T] {
  def sin(x: T): T

  def cos(x: T): T

  def exp(x: T): T

  def log(x: T): T

  def pow(x: T, n: T): T

  def powi(x: T, n: Int): T

  def sigmoid(x: T): T
}
