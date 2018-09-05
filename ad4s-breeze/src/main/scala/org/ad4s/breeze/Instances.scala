package org.ad4s.breeze

import org.ad4s.core.{Backprop, Kernel, Maths}

trait Instances {

  import breeze.linalg._

  implicit val denseMatrixDoubleKernel: Kernel[DenseMatrix[Double]] = new Kernel[DenseMatrix[Double]] {
    override def times(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
      a *:* b // element wise

    override def plus(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
      a + b

    override def minus(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
      a - b

    override def div(a: DenseMatrix[Double], b: DenseMatrix[Double]): DenseMatrix[Double] =
      a / b

    override def one(a: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix.ones(a.rows, a.cols)

    override def zero(t: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix.zeros(t.rows, t.cols)

    override def negate(t: DenseMatrix[Double]): DenseMatrix[Double] =
      -t

    override def fromInt(t: DenseMatrix[Double], i: Int): DenseMatrix[Double] =
      DenseMatrix.fill(t.rows, t.cols)(i)
  }

  import breeze.{numerics => num}

  implicit val denseMatrixDoubleMaths: Maths[DenseMatrix[Double]] = new Maths[DenseMatrix[Double]] {
    override def sin(x: DenseMatrix[Double]): DenseMatrix[Double] =
      num.sin(x)

    override def cos(x: DenseMatrix[Double]): DenseMatrix[Double] =
      num.cos(x)

    override def exp(x: DenseMatrix[Double]): DenseMatrix[Double] =
      num.exp(x)

    override def log(x: DenseMatrix[Double]): DenseMatrix[Double] =
      num.log(x)

    override def pow(x: DenseMatrix[Double], n: DenseMatrix[Double]): DenseMatrix[Double] =
      num.pow(x, n)

    override def powi(x: DenseMatrix[Double], n: Int): DenseMatrix[Double] =
      num.pow(x, n)

    override def sigmoid(x: DenseMatrix[Double]): DenseMatrix[Double] =
      num.sigmoid(x)
  }

  // this can be derived from Kernel
  implicit val denseMatrixDoubleBackprop:Backprop[DenseMatrix[Double]] = new Backprop[DenseMatrix[Double]] {
    override def zeros(a: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix.zeros(a.rows, a.cols)

    override def ones(a: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix.ones(a.rows, a.cols)

    override def add(a1: DenseMatrix[Double], a2: DenseMatrix[Double]): DenseMatrix[Double] =
      a1 + a2
  }
}

object Instances extends Instances
