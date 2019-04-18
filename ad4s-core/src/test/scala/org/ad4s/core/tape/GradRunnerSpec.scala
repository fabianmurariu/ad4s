package org.ad4s.core.tape

import cats.effect.IO
import org.scalatest.{FlatSpec, Matchers}

class GradRunnerSpec extends FlatSpec with Matchers{

  import cats.implicits._
  import cats.effect.implicits._

  "gradRunner" should "run the grads using FP" in {

    val sample = (1 to 9).map(_.toString).toList

    val possible = sample.foldM(0){(count, s) => IO.pure(count + s.length)}

    possible.unsafeRunSync() should be(9)

  }

}
