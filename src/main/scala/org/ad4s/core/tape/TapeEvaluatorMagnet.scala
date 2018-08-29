package org.ad4s.core.tape

import org.ad4s.core.{Backprop, Bv}

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
    // TODO: Add more
  }

}
