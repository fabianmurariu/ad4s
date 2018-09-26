package org.ad4s.core.backprop

import org.ad4s.core.tape._

case class Bv[+T](i: Int, v: T)

object Bv {

  def apply[T](v: T)(implicit BC: BackpropContext, Z:Zeros[T]): Bv[T] = {
    val node = Node0(zero = Z.zeros(v))
    val idx = BC.insertNode(node).unsafeRunSync()
    new Bv(idx, v)
  }

}