package org.ad4s.core.backprop

import org.ad4s.core.tape.BackpropContext

sealed trait BvFun

trait BvFun1[-A, +R] extends (Bv[A] => BackpropContext => Bv[R]) {
  def fapply(a: Bv[A])(implicit B: BackpropContext): Bv[R]

  override def apply(a: Bv[A]): BackpropContext => Bv[R] =
    bc => fapply(a)(bc)
}

trait BvFun2[-A, -B, +R] extends ((Bv[A], Bv[B]) => BackpropContext => Bv[R]) {
  def fapply(a: Bv[A], b: Bv[B])(implicit B: BackpropContext): Bv[R]

  override def apply(a: Bv[A], b: Bv[B]): BackpropContext => Bv[R] =
    bc => fapply(a, b)(bc)
}