package org.ad4s

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Properties
import shapeless.{HNil, ::}


object OpSpec extends Properties("Op") {

  import org.ad4s.Op._

  property("no op for doubles") = forAll {
    d: Double =>
      val actual = runOp(op0(d))(HNil)
      actual == (d -> HNil)
  }

  property("square for doubles") = forAll {
    d: Double =>
      val actual: (Double, Double :: HNil) = runOp(square[Double])(d :: HNil)
      val expected = d * d -> (2 * d :: HNil)
      (actual == expected) :| s"$actual != $expected"
  }

  property("multiplication for doubles") = forAll {
    (x: Double, y: Double) =>
      val actual = runOp(mul[Double])(x :: y :: HNil)
      val expected = (x * y) -> (y :: x :: HNil)
      (actual == expected) :| s"$actual != $expected"
  }

  property("compose (x + y) * (z^2)") = forAll {
    (x: Double, y: Double, z: Double) =>
      val opComposed = new CombineOpHelper(plus[Double], square[Double])(mul[Double]).apply()
      val (actualValue, actualGrads) = runOp(opComposed)(x :: y :: z :: HNil)
      val expectedEval = (x + y) * (z * z)
      val expectedGrads = (z * z) :: (z * z) :: (x + y) * 2 * z :: HNil
      (actualValue == expectedEval) :| s"$actualValue != $expectedEval" &&
        (actualGrads == expectedGrads) :| s"$actualGrads != $expectedGrads"
  }
}
