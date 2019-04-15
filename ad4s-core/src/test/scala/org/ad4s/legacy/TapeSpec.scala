//package org.ad4s.legacy
//
//import cats.Eval
//import org.scalatest.{FlatSpec, Matchers}
//
//class TapeSpec extends FlatSpec with Matchers {
//
//  "Tape" should "create variable" in {
//    val t = Tape.empty[Double]
//    t.push2(0, 1.2d)
//  }
//
//  it should "lift, sum 2 variables and apply to tape" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(0.5d)
//    val y = t._varS(4.2d)
//    val z = (x + y) * sin(x)
//    val (newTape, out) = z.run(t).value
//    out should be(Var((0.5d + 4.2d) * Math.sin(0.5), 4))
//  }
//
//  it should "return the derivates and value for x * y" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(0.5d)
//    val y = t._varS(4.2d)
//    val z = x * y
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(0.5d * 4.2d, 2))
//    grads.derivs(0) should be(4.2d)
//    grads.derivs(1) should be(0.5d)
//  }
//
//  it should "return the derivates and value for x + y" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(0.5d)
//    val y = t._varS(4.2d)
//    val z = x + y
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(0.5d + 4.2d, 2))
//    grads.derivs(0) should be(1d)
//    grads.derivs(1) should be(1d)
//  }
//
//  it should "return the derivates and value for sin(x)" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(0.5d)
//    val z = sin(x)
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(Math.sin(0.5), 1))
//    grads.derivs(0) should be(Math.cos(0.5))
//  }
//
//  it should "return the derivates and the value for (x + y) * sin(x)" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(0.5d)
//    val y = t._varS(4.2d)
//    val z = x + (y * sin(x))
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(0.5d + 4.2d * Math.sin(0.5), 4))
//    val dzdx = grads.derivs(0)
//    val dzdy = grads.derivs(1)
//    dzdx should be(4.2 * Math.cos(0.5) + 1)
//    dzdy should be(Math.sin(0.5d))
//  }
//
//  it should "sum a vector of vars" in {
//    import NumKernel._
//    import Var._
//    import cats.instances.vector._
//
//    val t = Tape.empty[Double]
//    val vs = Vector(t._varS(1d), t._varS(2d), t._varS(3d))
//    val z = reduceSum(vs).get
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(1d + 2d + 3d, 4))
//    grads.derivs.take(3) should be(Vector(1d, 1d, 1d))
//  }
//
//  it should "get derivs for exponentials x**y" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(2d)
//    val y = t._varS(3d)
//    val z = x ** y
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(8d, 2))
//    grads.derivs(0) should be(12d)
//    grads.derivs(1) should be(Math.pow(2d, 3d) * Math.log(2d))
//
//  }
//
//  it should "get derivs for sum X + Y" in {
//    import NumKernel._
//    import Var._
//
//    val t = Tape.empty[Double]
//    val x = t._varS(1d)
//    val y = t._varS(2d)
//    val z = x + y
//    val (_, (zVal, grads)) = z.eval2.run(t).value
//    zVal should be(Var(3.0, 2))
//    grads.derivs(0) should be(1d)
//    grads.derivs(1) should be(1d)
//
//  }
//
//  "Eval" should "retain the value of the function and not call it twice" in {
//    var x: Int = -1
//    val e = Eval.later {
//      x += 1 // state change
//      x
//    }
//    e.value should be(0)
//    e.value should be(0)
//    x should be(0)
//  }
//
//  it should "call the function twice on always" in {
//    var x: Int = -1
//    val e = Eval.always {
//      x += 1 // state change
//      x
//    }
//    e.value should be(0)
//    e.value should be(1)
//    x should be(1)
//  }
//
//}
