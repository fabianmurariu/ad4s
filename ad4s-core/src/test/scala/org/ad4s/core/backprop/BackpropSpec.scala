package org.ad4s.core.backprop

import cats.tests.CatsSuite
import org.ad4s.core.backprop.Backprop.implicits.backpropFromFractional
import org.ad4s.core.backprop.laws.discpline.BackpropTests

class BackpropSpec extends CatsSuite{

  checkAll("Backprop Double", BackpropTests[Double].backprop)

}
