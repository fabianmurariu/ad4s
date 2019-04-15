//package org.ad4s.legacy
//
//import org.ad4s.legacy.Op.Zero._
//import org.ad4s.legacy.Op._
//import org.scalacheck.Prop.{BooleanOperators, forAll}
//import org.scalacheck.Properties
//import shapeless.{::, HNil}
//object OpSpec extends Properties("Op") {
//
//  property("no op for doubles") = forAll {
//    d: Double =>
//      val actual = runOp(op0(d))(HNil)
//      actual == (d -> HNil)
//  }
//
//  property("const op returns 0 on all inputs for gradients") = forAll {
//    x: Double =>
//      val value = opConst[Double :: Double :: HNil, Double](x)
//      val actual = runOp(value)(1d :: 2d :: HNil)
//      val expected = (x, 0d :: 0d :: HNil)
//      (actual == expected) :| s"$actual != $expected"
//  }
//
//  property("square for doubles") = forAll {
//    d: Double =>
//      val actual: (Double, Double :: HNil) = runOp(square[Double])(d :: HNil)
//      val expected = d * d -> (2 * d :: HNil)
//      (actual == expected) :| s"$actual != $expected"
//  }
//
//  property("multiplication for doubles") = forAll {
//    (x: Double, y: Double) =>
//      val actual = runOp(mul[Double])(x :: y :: HNil)
//      val expected = (x * y) -> (y :: x :: HNil)
//      (actual == expected) :| s"$actual != $expected"
//  }
//
//  property("compose (x + y) * (z^2)") = forAll {
//    (x: Double, y: Double, z: Double) =>
//      val opComposed = new CombineOpHelper(plus[Double], square[Double])(mul[Double]).apply()
//      val (actualValue, actualGrads) = runOp(opComposed)(x :: y :: z :: HNil)
//      val expectedEval = (x + y) * (z * z)
//      val expectedGrads = (z * z) :: (z * z) :: (x + y) * 2 * z :: HNil
//      (actualValue == expectedEval) :| s"$actualValue != $expectedEval" &&
//        (actualGrads == expectedGrads) :| s"$actualGrads != $expectedGrads"
//  }
//}
