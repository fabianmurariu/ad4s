package org.ad4s.core

trait Kernel[T] {
  def times(a:T, b:T):T
  def plus(a:T, b:T):T
  def minus(a:T, b:T):T
  def one:T
  def zero(t:T):T
}
