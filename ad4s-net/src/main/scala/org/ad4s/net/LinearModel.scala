package org.ad4s.net

import breeze.linalg.DenseMatrix
import cats._
import cats.implicits._
import org.ad4s.core.backprop.BvMaths.ops._
import org.ad4s.core.backprop.{Backprop, d}
import org.ad4s.core.op.Ops._
import org.ad4s.breeze.DenseMatrixOps.ops._
import Backprop.implicits._

/**
  * Experimental implementation of a few models driven by
  * the generic batch type B[_]
  *
  * @tparam B
  * a batch
  */
abstract class LinearModel[B[_]](implicit M: Traverse[B] with Applicative[B]) {
  type Tensor = DenseMatrix[Double]

  case class Dense(W: d[Tensor], b: d[Tensor]) {
    def apply(xs: B[d[Tensor]]): B[d[Tensor]] =
      xs.map(x => (W * x) + b)
  }

  object Mse {
    def apply(pred: B[d[Tensor]], t: B[d[Tensor]]): d[Tensor] = {
      val n = pred.size
      assert(n > 0 && n == t.size)
      val sum: d[Tensor] = Apply[B].tuple2(pred, t).map[d[Tensor]] {
        case (x, y) =>
          val err = x - y
          err * err
      }.reduceLeftOption(_ + _).get
      sum / const(n.toDouble)
    }
  }

}
