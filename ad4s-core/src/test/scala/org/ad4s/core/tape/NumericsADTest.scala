package org.ad4s.core.tape

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import TapeEvaluatorMagnet.Implicits._
import org.ad4s.core.backprop.d
import org.scalacheck.Prop.BooleanOperators

import spire.implicits._
import org.ad4s.core.backprop.Backprop.implicits.backpropFromFractional
import org.ad4s.core.backprop.BvMaths.ops._
import org.ad4s.core.numeric.NumericOps.ops._

class NumericsADTest extends FlatSpec with Checkers {

  "Tape.runGrads" should "return (dx/dz, dy/dz) as (1, 1) for z=x+y " in check { (a: Double, b: Double) =>
    val f = (x: d[Double], y: d[Double]) => {
      x + y
    }

    val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
    val expectedZ = a + b
    (dx == 1d && dy == 1d) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
  }

  it should "return (dx/dz, dy/dz) as (1, 1) for z=x-y" in check { (a: Double, b: Double) =>
    val f = (x: d[Double], y: d[Double]) => {
      x - y
    }

    val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
    val expectedZ = a - b
    (dx == 1d && dy == -1d) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
  }

  it should "return (dx/dz, dy/dz) as (y, x)" in check { (a: Double, b: Double) =>
    val f = (x: d[Double], y: d[Double]) => {
      x * y
    }

    val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
    val expectedZ = a * b
    (z == expectedZ && dx == b && dy == a) :| s" $dx != $b | $dy != $a, $z != $expectedZ"
  }

  it should "return dx/dz as correct derivative for sigmoid(x)" in check {
    x: Double =>
      (Math.abs(x) < 100) ==> {
        val f = (x: d[Double]) => {
          sigmoid(x)
        }

        val (z, dx) = Tape.runGrads(f)(x)
        val expected = Math.exp(-x) / Math.pow(Math.exp(-x) + 1, 2)
        val expectedZ = 1 / (Math.exp(-x) + 1)
        (z == expectedZ && dx == expected) :| s"$dx != $expected, $z != $expectedZ"
      }
  }

  it should "return dx/dz as cos(x)" in check {
    x: Double =>
      val f = (x: d[Double]) => {
        sin(x)
      }

      val (z, dx) = Tape.runGrads(f)(x)
      val expected = Math.cos(x)
      val expectedZ = Math.sin(x)
      (z == expectedZ && dx == expected) :| s"$dx != $expected, $z != $expectedZ"
  }

  it should "return dx/dz as y*x**y-1m dy/dz as x**y*log(x)" in check {
    (a: Double, b: Double) =>
      (a > 0) ==> {
        val f = (x: d[Double], y: d[Double]) => {
          x ** y
        }

        val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
        val expectedDx = b * Math.pow(a, b - 1)
        val expectedDy = Math.pow(a, b * Math.log(a))
        val expectedZ = Math.pow(a, b + 0)
        (z == expectedZ && dx == expectedDx && dy == expectedDy) :| s" $dx != $expectedDx, $dy != $expectedDy, $z != $expectedZ"
      }
  }

  it should "return dx/dz as exp(x)" in check {
    x: Double =>

      val f = (x: d[Double]) => {
        exp(x)
      }

      val (z, dx) = Tape.runGrads(f)(x)
      val expected = Math.exp(x)
      val expectedZ = Math.exp(x)
      (z == expectedZ && dx == expected) :| s"$dx != $expected, $z != $expectedZ"

  }

  it should "return dx/dz as y+cos(x) and dy/dz as x" in check {
    (a: Double, b: Double) =>
      val f = (x: d[Double], y: d[Double]) => {
        (x * y) + sin(x)
      }
      val (z, (dx, dy)) = Tape.runGrads(f)((a, b))
      val expectedDx = b + Math.cos(a)
      val expectedDy = a
      val expectedZ = (a * b) + Math.sin(a)
      (z == expectedZ && dx == expectedDx && dy == expectedDy) :| s" $dx != $expectedDx, $dy != $expectedDy, $z != $expectedZ"
  }

  it should "return 1, 1, 1, 1 as dxs for sum" in check {
    (a: Double, b: Double, c: Double, d: Double) =>
      val f = (x: d[Double], y: d[Double], u: d[Double], v: d[Double]) => {
        x + y + u + v
      }
      val (z, dxs) = Tape.runGrads(f)((a, b, c, d))
      val expectedZ = a + b + c + d
      val expectedDxs = (1d, 1d, 1d, 1d)
      (z == expectedZ && dxs == expectedDxs) :| s" $dxs != $expectedDxs, $z != $expectedZ"
  }
  // Add more tests here
}
