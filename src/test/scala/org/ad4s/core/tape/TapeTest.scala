package org.ad4s.core.tape

import org.ad4s.core.Bv
import org.scalatest.FlatSpec
import org.scalatest.check.Checkers
import Bv.Implicits._
import BackpropContext.Implicits._
import org.scalacheck.Prop.BooleanOperators

class TapeTest extends FlatSpec with Checkers {

  "Backprop" should "return (dx/dz, dy/dz) as (1, 1) " in check { (a: Double, b: Double) =>
    val f = (x: Bv[Double], y: Bv[Double]) => { implicit BC: BackpropContext[Double] =>
      x + y
    }

    val (dx, dy) = Tape.runGrads(f)(a, b)
    dx == 1d && dy == 1d
  }

  it should "return (dx/dz, dy/dz) as (y, x)" in check { (a: Double, b:Double) =>
    val f = (x: Bv[Double], y: Bv[Double]) => { implicit BC: BackpropContext[Double] =>
      x * y
    }

    val (dx, dy) = Tape.runGrads(f)(a, b)
    (dx == b && dy == a) :| s" $dx != $b | $dy != $a"
  }
}
