package org.ad4s

package object core {

  implicit class KernelOps[T](val a: T) extends AnyVal {

    def +(b: T)(implicit K: Kernel[T]) =
      K.plus(a, b)

  }

}
