package org.ad4s.core.tape

import org.ad4s.core.backprop.{Backprop, DRef, Zeros, d}

import scala.language.implicitConversions

trait TapeEvaluatorMagnet[A, B] {
  def eval(x: A)
          (implicit BC: BackpropContext, B: Backprop[B]): (d[B], Grad => A)
}

object TapeEvaluatorMagnet {

  object Implicits {
    implicit def liftFn1IntoMagnet[A:Zeros, B](f: d[A] => d[B]) = new TapeEvaluatorMagnet[A, B] {

      override def eval(x: A)
                       (implicit BC: BackpropContext, BB: Backprop[B]): (d[B], Grad => A) = {
        val bv1 = d(x)
        val bvOut = f(bv1)
        (bvOut, grad => grad.dxs(0).asInstanceOf[A])
      }
    }

    implicit def liftFn2IntoMagnet[A:Zeros, B:Zeros, C](f: (d[A], d[B]) => d[C]) = new TapeEvaluatorMagnet[(A, B), C] {

      override def eval(x: (A, B))
                       (implicit BC: BackpropContext, B: Backprop[C]): (d[C], Grad => (A, B)) = {

        val bv1 = d(x._1)
        val bv2 = d(x._2)
        val bvOut = f(bv1, bv2)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[A], grad.dxs(1).asInstanceOf[B]))
      }
    }

    implicit def liftFn3IntoMagnet[T](f: (d[T], d[T], d[T]) => d[T]) = new TapeEvaluatorMagnet[(T, T, T), T] {

      override def eval(x: (T, T, T))
                       (implicit BC: BackpropContext, B: Backprop[T]): (d[T], Grad => (T, T, T)) = {

        val bv1 = d(x._1)
        val bv2 = d(x._2)
        val bv3 = d(x._3)
        val bvOut = f(bv1, bv2, bv3)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[T],
          grad.dxs(1).asInstanceOf[T],
          grad.dxs(2).asInstanceOf[T]))
      }
    }

    implicit def liftFn4IntoMagnet[T](f: (d[T], d[T], d[T], d[T]) => d[T]) = new TapeEvaluatorMagnet[(T, T, T, T), T] {

      override def eval(x: (T, T, T, T))
                       (implicit BC: BackpropContext, B: Backprop[T]): (d[T], Grad => (T, T, T, T)) = {

        val bv1 = d(x._1)
        val bv2 = d(x._2)
        val bv3 = d(x._3)
        val bv4 = d(x._4)
        val bvOut = f(bv1, bv2, bv3, bv4)
        (bvOut, grad => (grad.dxs(0).asInstanceOf[T],
          grad.dxs(1).asInstanceOf[T],
          grad.dxs(2).asInstanceOf[T],
          grad.dxs(3).asInstanceOf[T]))
      }
    }

    // TODO: Add more
  }

}
