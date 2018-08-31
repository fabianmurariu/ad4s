package org.ad4s.core.tape

import org.ad4s.core.{Backprop, Bv}
import scala.language.implicitConversions

trait TapeEvaluatorMagnet[X, Z] {
  type Grads

  def eval(x: X)
          (implicit BC: BackpropContext[Z], B: Backprop[Z]): (Bv[Z], Grad => Grads)
}

object TapeEvaluatorMagnet {
  type BvOut[T] = BackpropContext[T] => Bv[T]

  object Implicits {
    implicit def liftFn1IntoMagnet[T](f: Bv[T] => BvOut[T]) = new TapeEvaluatorMagnet[T, T] {
      override type Grads = T

      override def eval(x: T)
                       (implicit BC: BackpropContext[T], B: Backprop[T]): (Bv[T], Grad => T) = {
        val bv1 = Bv(x)
        val bvOut = f(bv1)(BC)
        (bvOut, grad => grad.dxs(0).asInstanceOf[T])
      }
    }

    implicit def liftFn2IntoMagnet[T](f: (Bv[T], Bv[T]) => BvOut[T]) = new TapeEvaluatorMagnet[(T, T), T] {
      override type Grads = (T, T)

      override def eval(x: (T, T))
                       (implicit BC: BackpropContext[T], B: Backprop[T]): (Bv[T], Grad => (T, T)) = {

        val bv1 = Bv(x._1)
        val bv2 = Bv(x._2)
        val bvOut = f(bv1, bv2)(BC)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[T], grad.dxs(1).asInstanceOf[T]))
      }
    }

    implicit def liftFn3IntoMagnet[T](f: (Bv[T], Bv[T], Bv[T]) => BvOut[T]) = new TapeEvaluatorMagnet[(T, T, T), T] {
      override type Grads = (T, T, T)

      override def eval(x: (T, T, T))
                       (implicit BC: BackpropContext[T], B: Backprop[T]): (Bv[T], Grad => (T, T, T)) = {

        val bv1 = Bv(x._1)
        val bv2 = Bv(x._2)
        val bv3 = Bv(x._3)
        val bvOut = f(bv1, bv2, bv3)(BC)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[T],
          grad.dxs(1).asInstanceOf[T],
          grad.dxs(2).asInstanceOf[T]))
      }
    }

    implicit def liftFn4IntoMagnet[T](f: (Bv[T], Bv[T], Bv[T], Bv[T]) => BvOut[T]) = new TapeEvaluatorMagnet[(T, T, T, T), T] {
      override type Grads = (T, T, T, T)

      override def eval(x: (T, T, T, T))
                       (implicit BC: BackpropContext[T], B: Backprop[T]): (Bv[T], Grad => (T, T, T, T)) = {

        val bv1 = Bv(x._1)
        val bv2 = Bv(x._2)
        val bv3 = Bv(x._3)
        val bv4 = Bv(x._4)
        val bvOut = f(bv1, bv2, bv3, bv4)(BC)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[T],
          grad.dxs(1).asInstanceOf[T],
          grad.dxs(2).asInstanceOf[T],
          grad.dxs(3).asInstanceOf[T]))
      }
    }

    // TODO: Add more
  }

}
