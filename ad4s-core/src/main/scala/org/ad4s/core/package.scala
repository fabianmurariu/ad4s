package org.ad4s

package object core {

  implicit class KernelOps[T](val a: T) extends AnyVal {

    def +(b: T)(implicit K: Kernel[T]): T =
      K.plus(a, b)

    def -(b: T)(implicit K: Kernel[T]): T =
      K.minus(a, b)

    def *(b: T)(implicit K: Kernel[T]): T =
      K.times(a, b)

    def /(b: T)(implicit K: Kernel[T]): T =
      K.div(a, b)

    def unary_-(implicit K:Kernel[T]): T = K.negate(a)

  }


}
