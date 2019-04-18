package org.ad4s.breeze

import breeze.linalg
import breeze.linalg.{DenseMatrix, sum}
import org.ad4s.breeze.DenseMatrixOps.ops._
import org.ad4s.core.backprop.BvMaths.ops._
import org.ad4s.core.backprop.d
import org.ad4s.core.backprop.Backprop.implicits.backpropFromFractional
import org.ad4s.core.tape.Tape
import org.ad4s.core.tape.TapeEvaluatorMagnet.Implicits._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers

class BreezeADTest extends FlatSpec with Checkers with Matchers {
  type Tensor = DenseMatrix[Double]
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

  def one(x: Tensor): Tensor =
    DenseMatrix.ones(x.rows, x.cols)

  "z=x+y" should "return (dx/dz, dy/dz) as (1, 1)" in {
    implicit val denseMatrixDoubleMulArb: Arbitrary[(Tensor, Tensor)] = Arbitrary(gen)
    check { pair: (Tensor, Tensor) =>
      val f = (x: d[Tensor], y: d[Tensor]) => {
        x + y
      }

      val (a, b) = pair
      val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
      val expectedZ = a + b
      (dx == one(a) && dy == one(b)) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
    }
  }

  "z=x*y" should "return (dx/dz, dy/dz)" in {
    implicit val denseMatrixDoubleMulArb: Arbitrary[(Tensor, Tensor)] = Arbitrary(genMul)
    check { pair: (Tensor, Tensor) =>
      val f = (x: d[Tensor], y: d[Tensor]) => {
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

  "z=x/y" should "return (dx/dz, dy/dz)" in {
    val f = (x: d[Tensor], y: d[Double]) => {
      x / y
    }
    val a = DenseMatrix.create(2, 2, Array(2d, 3d, 4d, 5d))
    val b = 3d
    val (z, (dx, dy)) = Tape.runGrads(f)((a, b))

    assert(dx == DenseMatrix.create(2, 2, Array(1d, 1d, 1d, 1d).map(_ / 3d)))
    dy should ===(-1.5556d +- 0.001)

    implicit val denseMatrixDoubleMulArb: Arbitrary[Tensor] = Arbitrary(genMul.map(_._1))
    check { pair: (Tensor, Double) =>
      val (a, b) = pair

      val (z, (dx, dy)) = Tape.runGrads(f)((a, b))

      val expectedZ = a / b
      val expDx = 1 / b * one(a)
      val expDy: Double = sum((-1 / Math.pow(b, 2)) * a)
      (dx == expDx && z == expectedZ && expDy == dy) :| s" $z != $expectedZ | $dx != $expDx | $dy != $expDy"
    }
  }

  "det(x)" should "return determinant and correct deriv for determinant G * C * (A**-1).T as (y, z) for " in {
    implicit val squareMatrixArb: Arbitrary[Tensor] = Arbitrary(squares)
    check { m: Tensor =>

      val f = (x: d[Tensor]) => {
        det[Tensor, Double](x)
      }

      val (z, dx) = Tape.runGrads(f)(m)
      val expectedZ = linalg.det(m)
      val expDx = expectedZ * linalg.inv(m).t
      (z == expectedZ && dx == expDx) :| s" $dx != $expDx | $z != $expectedZ"

    }
  }

}
