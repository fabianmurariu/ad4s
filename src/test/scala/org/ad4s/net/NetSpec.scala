package org.ad4s.net

import org.scalatest.{FlatSpec, Matchers}

/**
  * Given 2 gaussian distributions in top right and bottom left
  * given (x,y) coordinate system
  * a line Wx + b = y
  * a cost function J = (y' - (Wx + b))**2
  * figure out W and b given (X, Y)
  * such that J is minimum
  */
class NetSpec extends FlatSpec with Matchers {

  "Net" should "train" in {
    import org.ad4s.tape.NumKernel._
    import org.ad4s.tape.Var._

    val X = Vector(3.3, 4.4, 5.5, 6.71, 6.93, 4.168, 9.779, 6.182, 7.59, 2.167,
      7.042, 10.791, 5.313, 7.997, 5.654, 9.27, 3.1)

    val Y = Vector(1.7, 2.76, 2.09, 3.19, 1.694, 1.573, 3.366, 2.596, 2.53, 1.221,
      2.827, 3.465, 1.65, 2.904, 2.42, 2.94, 1.3)

    def pred(W: LVar[Double], b: LVar[Double], x: LVar[Double]): LVar[Double] = {
      (W * x) + b
    }

//    def cost(pred:LVar[Double], y:LVar[Double]):LVar[Double] = {
//      (y - pred) ** liftVar(2)
//    }

  }

}
