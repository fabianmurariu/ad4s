package org.ad4s.breeze

import breeze.linalg
import breeze.linalg.DenseMatrix
import org.ad4s.breeze.DenseMatrixOps.ops._
import org.ad4s.core.backprop.Bv
import org.ad4s.core.backprop.BvMaths.ops._
import org.ad4s.core.numeric.NumericOps.ops.backpropFromFractional
import org.ad4s.core.tape.TapeEvaluatorMagnet.Implicits._
import org.ad4s.core.tape.{BackpropContext, Tape}
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FlatSpec
import org.scalatest.check.Checkers

class BreezeADTest extends FlatSpec with Checkers {

  // matrices of the same dimensions
  val gen = for {
    r <- Gen.choose(1, 3)
    c <- Gen.choose(1, 3)
    doubles1 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
    doubles2 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
  } yield (DenseMatrix.create(r, c, doubles1.toArray), DenseMatrix.create(r, c, doubles2.toArray))

  val genMul = for {
    r <- Gen.choose(1, 3)
    c <- Gen.choose(1, 3)
    doubles1 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
    doubles2 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
  } yield (DenseMatrix.create(c, r, doubles1.toArray), DenseMatrix.create(r, c, doubles2.toArray))

  val squares = for {
    d <- Gen.choose(1, 5)
    doubles1 <- Gen.containerOfN[Vector, Double](d * d, Gen.choose(1d, 10d))
  } yield DenseMatrix.create(d, d, doubles1.toArray)

  def one(x: DenseMatrix[Double]): DenseMatrix[Double] =
    DenseMatrix.ones(x.rows, x.cols)

  "z=x+y" should "return (dx/dz, dy/dz) as (1, 1)" in {
    implicit val denseMatrixDoubleMulArb: Arbitrary[(DenseMatrix[Double], DenseMatrix[Double])] = Arbitrary(gen)
    check { pair: (DenseMatrix[Double], DenseMatrix[Double]) =>
      val f = (x: Bv[DenseMatrix[Double]], y: Bv[DenseMatrix[Double]]) => { implicit BC: BackpropContext =>
        x + y
      }

      val (a, b) = pair
      val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
      val expectedZ = a + b
      (dx == one(a) && dy == one(b)) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
    }
  }

  "z=x*y" should "return (dx/dz, dy/dz) as (y, z) for " in {
    implicit val denseMatrixDoubleMulArb: Arbitrary[(DenseMatrix[Double], DenseMatrix[Double])] = Arbitrary(genMul)
    check { pair: (DenseMatrix[Double], DenseMatrix[Double]) =>
      val f = (x: Bv[DenseMatrix[Double]], y: Bv[DenseMatrix[Double]]) => { implicit BC: BackpropContext =>
        x * y
      }
      val (a, b) = pair
      assert(a.cols == b.rows, "Dimension mismatch!")
      val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
      val expectedZ = a * b
      val expDx = one(z) * b.t
      val expDy = a.t * one(z)
      (dx == expDx && dy == expDy) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
    }
  }

  "det(x)" should "return determinant and correct deriv for determinant G * C * (A**-1).T as (y, z) for " in {
    implicit val squareMatrixArb: Arbitrary[DenseMatrix[Double]] = Arbitrary(squares)
    check { m: DenseMatrix[Double] =>

      val f: Bv[DenseMatrix[Double]] => BackpropContext => Bv[Double] = (x: Bv[DenseMatrix[Double]]) => { implicit BC: BackpropContext =>
        det[DenseMatrix[Double], Double](x)
      }

      val (z, dx) = Tape.runGrads(f)(m)
      val expectedZ = linalg.det(m)
      val expDx = expectedZ * linalg.inv(m).t
      (z == expectedZ && dx == expDx) :| s" $dx != $expDx | $z != $expectedZ"

    }
  }

}
