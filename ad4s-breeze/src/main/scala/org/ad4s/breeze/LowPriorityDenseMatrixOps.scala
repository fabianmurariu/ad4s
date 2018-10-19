package org.ad4s.breeze

import breeze.linalg.{DenseMatrix, det, inv, max, sum}
import breeze.numerics._
import org.ad4s.core.backprop.Backprop
import org.ad4s.core.op.Ops._

trait LowPriorityDenseMatrixOps {


  /* WIP */
  object ops {
    type Matrix = DenseMatrix[Double]

    implicit val plusOp: Plus[Matrix, Matrix, Matrix] = new Plus[Matrix, Matrix, Matrix] {
      override def apply(a: Matrix, b: Matrix): (Matrix, Matrix => (Matrix, Matrix)) =
        (a + b, { z => (z, z) })
    }

    implicit val minusOp: Minus[Matrix, Matrix, Matrix] = new Minus[Matrix, Matrix, Matrix] {
      override def apply(a: Matrix, b: Matrix): (Matrix, Matrix => (Matrix, Matrix)) =
        (a + b, { z => (z, -z) })
    }

    implicit val timesOp: Times[Matrix, Matrix, Matrix] = new Times[Matrix, Matrix, Matrix] {
      override def apply(a: Matrix, b: Matrix): (Matrix, Matrix => (Matrix, Matrix)) =
        (a * b, { z => (z * b.t, a.t * z) })
    }

    implicit val divOp: Div[Matrix, Double, Matrix] = new Div[Matrix, Double, Matrix] {
      override def apply(a: Matrix, b: Double): (Matrix, Matrix => (Matrix, Double)) =
        (a / b, { z => 
          val db = sum(z *:* ((-1 / Math.pow(b, 2)) * a))
          (z / b, db)
        })
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

    implicit val detOps: Det[Matrix, Double] = new Det[Matrix, Double] {
      override def apply(v1: Matrix): (Double, Double => Matrix) = {
        val d = det(v1)
        (d, g => (g * d) * inv(v1).t)
      }
    }

    implicit val maxOps: Max[Matrix, Double] = new Max[Matrix, Double] {
      override def apply(v1: Matrix): (Double, Double => Matrix) = {
        val m = max(v1)
        (m, g => {
          val dm = (DenseMatrix.fill(v1.rows, v1.cols)(m) :== v1).map(b => if (b) 1.0 else 0.0)
          g * dm
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