package org.ad4s.core.tape

import org.scalatest.check.Checkers
import org.scalatest.{FlatSpec, Matchers}

class TapeTest extends FlatSpec with Checkers{

  "Tape" should "be checked" in check { (a: List[Int], b: List[Int]) =>
    a.size + b.size == (a ::: b).size
  }

}
