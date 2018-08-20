package org.ad4s.core

trait Kernel[T] {
  def plus(a:T, b:T):T
  def minus(a:T, b:T):T
}
