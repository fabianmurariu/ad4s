package org.ad4s.core.numeric

trait Algebra {

  def pow[T](x:T, y:T)(implicit F:spire.math.Fractional[T]): T = F.fpow(x, y)

}

object Algebra extends Algebra
