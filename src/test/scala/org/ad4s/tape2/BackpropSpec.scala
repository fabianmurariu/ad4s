package org.ad4s.tape2

import org.scalatest.{FlatSpec, Matchers}

class BackpropSpec extends FlatSpec with Matchers {

  import org.ad4s.tape2.Backprop._
  import BVar._

  def testSum2BVar(W: Tape[Double])
                  (bv1: BVar[Double], bv2: BVar[Double]): BVar[Double] = {
    implicit val _ = W
    bv1 + bv2
  }

  "Op" should "support + operations" in {
    val (t, out) = fillWengert2[Double](testSum2BVar)(1d, 2d).unsafeRunSync()
    out should be(3d)
    t.tape.length should be(1)
    val Tape(Seq(SomeTapeNode(0.0d, TapeNode(inputs, gradFn)))) = t
    inputs.map(ir => ir.ix) should be(Seq(BVar(BRInp(0), 1d), BVar(BRInp(1), 2d)))
  }

  "Runner" should "init from tape and inputs" in {
    val (t, _) = fillWengert2[Double](testSum2BVar)(1d, 2d).unsafeRunSync()
    val actual = Runner(t, 1d, 2d)
    actual.rDelta should be(Seq(0d))
    actual.rInputs should be(Seq(1d, 2d))
  }

  it should "enable fill the gradients in the Runner" in pendingUntilFixed {
    val (t, out) = fillWengert2[Double](testSum2BVar)(1d, 2d).unsafeRunSync()
    val runner = Runner(t, 1d, 2d)
    gradRunner(1d, runner, t)
    // TODO: WTF!
    runner.rInputs shouldBe Seq(2d, 3d)
    runner.rDelta shouldBe Seq(1d)
    fail("not what I expected")
  }

}
