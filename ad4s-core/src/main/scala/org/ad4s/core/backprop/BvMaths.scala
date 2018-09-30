package org.ad4s.core.backprop

import cats.effect.IO
import org.ad4s.core.op.Op
import org.ad4s.core.op.Ops._

trait BvMaths {

  def plus[A: Sum, B: Sum, C: Zeros](a: d[A], b: d[B])
                                    (implicit P: Plus[A, B, C]): IO[d[C]] = {
    Op.liftOp2(a, b)(P)
  }

  def minus[A: Sum, B: Sum, C: Zeros](a: d[A], b: d[B])
                                     (implicit OP: Minus[A, B, C]): IO[d[C]] = {
    Op.liftOp2(a, b)(OP)
  }

  def times[A: Sum, B: Sum, C: Zeros](a: d[A], b: d[B])
                                     (implicit OP: Times[A, B, C]): IO[d[C]] = {
    Op.liftOp2(a, b)(OP)
  }

  def div[A: Sum, B: Sum, C: Zeros](a: d[A], b: d[B])
                                   (implicit OP: Div[A, B, C]): IO[d[C]] = {
    Op.liftOp2(a, b)(OP)
  }

  def pow[A: Sum, B: Sum, C: Zeros](a: d[A], b: d[B])
                                   (implicit OP: Pow[A, B, C]): IO[d[C]] = {
    Op.liftOp2(a, b)(OP)
  }

  // they return Bv[A]
  def sin[A](a: d[A])(implicit OP: Sin[A, A], Bp: Backprop[A]): d[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def sigmoid[A](a: d[A])(implicit OP: Sigmoid[A, A], Bp: Backprop[A]): d[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def cos[A](a: d[A])(implicit OP: Cos[A, A], Bp: Backprop[A]): d[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def exp[A](a: d[A])(implicit OP: Exp[A, A], Bp: Backprop[A]): d[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def log[A](a: d[A])(implicit OP: Log[A, A], Bp: Backprop[A]): d[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def det[A: Sum, B: Zeros](a: d[A])
                           (implicit OP: Det[A, B]): d[B] =
    Op.liftOp1(a)(OP).unsafeRunSync()

}

object BvMaths {

  object ops extends BvMaths {

    implicit class BvOps[A](val a: d[A]) extends AnyVal {
      def +(b: d[A])(implicit P: Plus[A, A, A],
                     Bp: Backprop[A]): d[A] = {
        plus(a, b).unsafeRunSync()
      }

      def -(b: d[A])(implicit P: Minus[A, A, A],
                     Bp: Backprop[A]): d[A] = {
        minus(a, b).unsafeRunSync()
      }

      def *(b: d[A])(implicit P: Times[A, A, A],
                     Bp: Backprop[A]): d[A] = {
        times(a, b).unsafeRunSync()
      }

      def /(b: d[A])(implicit P: Div[A, A, A],
                     Bp: Backprop[A]): d[A] = {
        div(a, b).unsafeRunSync()
      }

      def **(b: d[A])(implicit P: Pow[A, A, A],
                      Bp: Backprop[A]): d[A] = {
        pow(a, b).unsafeRunSync()
      }
    }

  }

}
