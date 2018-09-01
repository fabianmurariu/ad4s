package org.ad4s.net

import cats.implicits._
import org.ad4s.core.Bv
import org.ad4s.core.tape.BackpropContext
import org.scalatest.{FlatSpec, Matchers}

class NetSpec extends FlatSpec with Matchers {

  import BackpropContext.Implicits._
  import Bv.Implicits._
  import org.ad4s.core.Ops._


  "Net" should "take any foldable and update the variables via the optimizer" in pendingUntilFixed {
    import org.ad4s.net.Net.Implicits._

    val X = Vector[Double](3.3, 4.4, 5.5, 6.71, 6.93, 4.168, 9.779, 6.182, 7.59, 2.167,
      7.042, 10.791, 5.313, 7.997, 5.654, 9.27, 3.1)

    val Y = Vector[Double](1.7, 2.76, 2.09, 3.19, 1.694, 1.573, 3.366, 2.596, 2.53, 1.221,
      2.827, 3.465, 1.65, 2.904, 2.42, 2.94, 1.3)

    val cost = (W: Bv[Double], b: Bv[Double]) => (x: Bv[Double], y: Bv[Double]) => { implicit BC: BackpropContext[Double] =>
      val pred = W * x + b
      val J = pow(pred - y, Bv(2d)) // cost
      J
    }

    Net.run(X.zip(Y))(cost)
  }
}