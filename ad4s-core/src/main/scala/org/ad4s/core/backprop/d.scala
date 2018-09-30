package org.ad4s.core.backprop

import org.ad4s.core.tape._

case class d[+T](i: Ref, v: T)

sealed trait Ref

case object DConst extends Ref

case class DRef(i: Int, bc: BackpropContext) extends Ref


object d {

  def apply[T](v: T)(implicit Z: Zeros[T], bc: BackpropContext): d[T] = {
    val node = Node0(zero = Z.zeros(v))
    val idx = bc.insertNode(node).unsafeRunSync()
    d(DRef(idx, bc), v)
  }

  def const[T](v: T): d[T] = d(DConst, v)

}