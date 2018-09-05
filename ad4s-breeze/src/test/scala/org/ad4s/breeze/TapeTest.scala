package org.ad4s.breeze

import breeze.linalg.DenseMatrix
import Instances._
import org.ad4s.core.{Bv, Kernel}
import org.ad4s.core.Bv.Implicits._
import org.ad4s.core.tape.BackpropContext.Implicits._
import org.ad4s.core.tape.TapeEvaluatorMagnet.Implicits._
import org.ad4s.core.tape.{BackpropContext, Tape, TapeEvaluatorMagnet}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.FlatSpec
import org.scalatest.check.Checkers

class TapeTest extends FlatSpec with Checkers {

  // square matrices of the same dimensions
  val gen = for {
    r <- Gen.choose(1, 5)
    c <- Gen.choose(1, 5)
    doubles1 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
    doubles2 <- Gen.containerOfN[Vector, Double](r * c, Gen.choose(1d, 10d))
  } yield (DenseMatrix.create(r, c, doubles1.toArray), DenseMatrix.create(r, c, doubles2.toArray) )

  implicit val denseMatrixDoubleArb: Arbitrary[(DenseMatrix[Double], DenseMatrix[Double])] = Arbitrary(gen)

  "Backprop over DenseMatrix" should "return (dx/dz, dy/dz) as (1, 1) for z=x+y " in check { pair: (DenseMatrix[Double],DenseMatrix[Double]) =>
    val f = (x: Bv[DenseMatrix[Double]], y: Bv[DenseMatrix[Double]]) => { implicit BC: BackpropContext[DenseMatrix[Double]] =>
      x + y
    }
    val K = implicitly[Kernel[DenseMatrix[Double]]]
    val (a, b) = pair
    val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
    val expectedZ = a + b
    (dx == K.one(a) && dy == K.one(b)) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
  }

}
