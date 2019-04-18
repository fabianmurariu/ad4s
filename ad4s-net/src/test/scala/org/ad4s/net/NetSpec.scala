package org.ad4s.net

import org.ad4s.core.backprop.d
import org.ad4s.core.tape.Tape
import org.scalatest.{FlatSpec, Matchers}
import org.ad4s.core.backprop.Backprop.implicits.backpropFromFractional

class NetSpec extends FlatSpec with Matchers {

  import org.ad4s.core.backprop.BvMaths.ops._
  import org.ad4s.core.numeric.NumericOps.ops._
  import org.ad4s.core.tape.TapeEvaluatorMagnet.Implicits._
  import spire.implicits._

  "Net" should "take any foldable and update the variables via the gradient descent" in {

    val X = Vector[Double](3.3, 4.4, 5.5, 6.71, 6.93, 4.168, 9.779, 6.182, 7.59, 2.167,
      7.042, 10.791, 5.313, 7.997, 5.654, 9.27, 3.1)

    val Y = Vector[Double](1.7, 2.76, 2.09, 3.19, 1.694, 1.573, 3.366, 2.596, 2.53, 1.221,
      2.827, 3.465, 1.65, 2.904, 2.42, 2.94, 1.3)

    val cost = (W: d[Double], b: d[Double], x: d[Double], y: d[Double]) => {
      val pred = (W * x) + b
      // cost
      (pred - y) ** d.const(2d)
    }

    def repeat[T](n: Int, xs: Traversable[T]): Stream[T] = {
      Stream.continually(xs).take(n).flatten
    }

    // behold SGD with batch_size = 1
    val (weight, bias, _, c) = repeat(9000, X.zip(Y)).zipWithIndex.foldLeft((0.5, 0.5, 0.0001, 0.001)) {
      case ((w, b, lr, _), ((x, y), step)) =>
        val (z, (dw, db, _, _)) = Tape.runGrads(cost)((w, b, x, y))

        val newW = w - (lr * dw)
        val newB = b - (lr * db)
        (newW, newB, lr, z)
    }
    // from one of the Tensorflow runs
    // Epoch: 1000 cost= 0.077266559 W= 0.259104 b= 0.733052
    c should ===(0.077 +- 0.01)
    weight should ===(0.25 +- 0.01)
    bias should ===(0.8 +- 0.01)

  }
}