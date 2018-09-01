package org.ad4s.net

import cats.Foldable
import org.ad4s.core.{Backprop, Bv}
import org.ad4s.core.tape.{BackpropContext, Tape, TapeEvaluatorMagnet}

trait Net[W, X, Y] extends Serializable {

  def weights: W

  def forward(x: X): Bv[Y]

}

object Net {

  def run[X, F[_], W, Y](xs: F[X])
                        (f: Net[W, X, Y])
                        (implicit FL: Foldable[F], B: Backprop[Y]) = {
    FL.foldLeft(xs, Option.empty[Bv[Y]]) {
      (prevY, x) =>
        implicit val BC = new BackpropContext[Y]
        val y = f.forward(x)
        Some(y)
    }
  }

  object Implicits {
    type BvOut[T] = BackpropContext[T] => Bv[T]

    implicit def liftFn4ToNet[T](f: (Bv[T], Bv[T]) => (Bv[T], Bv[T]) => BvOut[T])
                                (implicit B: Backprop[T]): Net[(T, T), (T, T), T] = new Net[(T, T), (T, T), T] {

      override def weights: (T, T) = ???

      override def forward(x: (T, T)): Bv[T] = ???
    }
  }

}
