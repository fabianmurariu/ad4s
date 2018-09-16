package org.ad4s.breeze

import breeze.linalg.DenseMatrix
import org.ad4s.core.backprop.Backprop
import org.ad4s.core.op.Ops.{Exp, Plus, Sigmoid, Times}
import breeze.numerics._

trait LowPriorityDenseMatrixOps {

  /* WIP */
  object ops {
    type Matrix = DenseMatrix[Double]

    implicit val plusOp: Plus[Matrix, Matrix, Matrix] = new Plus[Matrix, Matrix, Matrix] {
      override def apply(a: Matrix, b: Matrix): (Matrix, Matrix => (Matrix, Matrix)) =
        (a + b, { z => (z, z) })
    }

    implicit val timesOp: Times[Matrix, Matrix, Matrix] = new Times[Matrix, Matrix, Matrix] {
      override def apply(a: Matrix, b: Matrix): (Matrix, Matrix => (Matrix, Matrix)) =
        (a * b, { z => (z * b.t, a.t * z) })
    }

    implicit val expOps: Exp[Matrix, Matrix] = new Exp[Matrix, Matrix] {
      override def apply(a: Matrix): (Matrix, Matrix => Matrix) =
        (exp(a), g => g * exp(a))
    }

    implicit val sigmoidOps: Sigmoid[Matrix, Matrix] = new Sigmoid[Matrix, Matrix] {
      override def apply(a: Matrix): (Matrix, Matrix => Matrix) = {
        (sigmoid(a), {
          g =>
            val ex = exp(-a)
            val ones = DenseMatrix.ones[Double](ex.rows, ex.cols)
            g * (ex / pow(ex + ones, 2))
        })
      }
    }

    implicit val backpropForDenseMatrix: Backprop[Matrix] = new Backprop[Matrix] {
      override def zeros(a: Matrix): Matrix = DenseMatrix.zeros[Double](a.rows, a.cols)

      override def add(a1: Matrix, a2: Matrix): Matrix = a1 + a2

      override def ones(a: Matrix): Matrix = DenseMatrix.ones[Double](a.rows, a.cols)
    }
  }

}

object DenseMatrixOps extends LowPriorityDenseMatrixOps