package org.ad4s.core.backprop

import org.ad4s.core.tape._

case class Bv[+T](i: Int, v: T)

object Bv {

  def apply[T](v: T)(implicit BC: BackpropContext, B: Backprop[T]): Bv[T] = {
    val node = Node(
      inputs = Seq.empty[InpRef[T]],
      zero = B.zeros(v),
      grad = { _: T => Seq.empty[T] }
    )
    val idx = BC.insertNode(node).unsafeRunSync()
    new Bv(idx, v)
  }

}