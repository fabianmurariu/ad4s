package org.ad4s.core.backprop

import cats.effect.IO
import org.ad4s.core.op.Op
import org.ad4s.core.op.Ops._
import org.ad4s.core.tape.BackpropContext

trait BvMaths {

  //TODO: figure out what these should return
  // they return IO[Bv[A]]
  def plus[A](a: Bv[A], b: Bv[A])
             (implicit P: Plus[A, A, A],
              Bp: Backprop[A],
              B: BackpropContext): IO[Bv[A]] = {
    Op.liftOp2(a, b)(P)
  }

  def minus[A](a: Bv[A], b: Bv[A])
              (implicit OP: Minus[A, A, A],
               B: Backprop[A],
               BC: BackpropContext): IO[Bv[A]] = {
    Op.liftOp2(a, b)(OP)
  }

  def times[A](a: Bv[A], b: Bv[A])
              (implicit OP: Times[A, A, A],
               B: Backprop[A],
               BC: BackpropContext): IO[Bv[A]] = {
    Op.liftOp2(a, b)(OP)
  }

  def div[A](a: Bv[A], b: Bv[A])
            (implicit OP: Div[A, A, A],
             B: Backprop[A],
             BC: BackpropContext): IO[Bv[A]] = {
    Op.liftOp2(a, b)(OP)
  }

  def pow[A](a: Bv[A], b: Bv[A])
            (implicit OP: Pow[A, A, A],
             B: Backprop[A],
             BC: BackpropContext): IO[Bv[A]] = {
    Op.liftOp2(a, b)(OP)
  }

  // they return Bv[A]
  def sin[A](a: Bv[A])(implicit OP: Sin[A, A], Bp: Backprop[A], B: BackpropContext): Bv[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def sigmoid[A](a: Bv[A])(implicit OP: Sigmoid[A, A], Bp: Backprop[A], B: BackpropContext): Bv[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def cos[A](a: Bv[A])(implicit OP: Cos[A, A], Bp: Backprop[A], B: BackpropContext): Bv[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def exp[A](a: Bv[A])(implicit OP: Exp[A, A], Bp: Backprop[A], B: BackpropContext): Bv[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def log[A](a: Bv[A])(implicit OP: Log[A, A], Bp: Backprop[A], B: BackpropContext): Bv[A] =
    Op.liftOp1(a)(OP).unsafeRunSync()

  def det[A:Sum, B:Zeros](a: Bv[A])(implicit OP: Det[A, B], B: BackpropContext): Bv[B] =
    Op.liftOp1(a)(OP).unsafeRunSync()

}

object BvMaths {

  object ops extends BvMaths {

    implicit class BvOps[A](val a: Bv[A]) extends AnyVal {
      def +(b: Bv[A])(implicit P: Plus[A, A, A],
                      Bp: Backprop[A],
                      B: BackpropContext): Bv[A] = {
        plus(a, b).unsafeRunSync()
      }

      def -(b: Bv[A])(implicit P: Minus[A, A, A],
                      Bp: Backprop[A],
                      B: BackpropContext): Bv[A] = {
        minus(a, b).unsafeRunSync()
      }

      def *(b: Bv[A])(implicit P: Times[A, A, A],
                      Bp: Backprop[A],
                      B: BackpropContext): Bv[A] = {
        times(a, b).unsafeRunSync()
      }

      def /(b: Bv[A])(implicit P: Div[A, A, A],
                      Bp: Backprop[A],
                      B: BackpropContext): Bv[A] = {
        div(a, b).unsafeRunSync()
      }

      def **(b: Bv[A])(implicit P: Pow[A, A, A],
                       Bp: Backprop[A],
                       B: BackpropContext): Bv[A] = {
        pow(a, b).unsafeRunSync()
      }
    }

  }

}
