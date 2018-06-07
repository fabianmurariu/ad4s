package org.ad4s.core

import cats.Eval
//import cats.data.State
//import org.ad4s.Op
//import org.ad4s.tape.Tape
//import shapeless.{::, HNil}

/**
  * value of type a that can be "backpropagated"
  *
  * @tparam S
  * underlying state
  * @tparam A
  * value
  */
case class BVar[S, A](ref: BRef[S], v: A)

sealed trait BRef[S]

case class BRInp[S](i: Eval[Int]) extends BRef[S]
case class BRIx[S](i: Eval[Int]) extends BRef[S]
case object BRC extends BRef[Nothing]

object BVar {

  //  def liftOp1[A: Backprop, B: Backprop, W <: Tape[B]](op: Op[A :: HNil, B]): BVar[W, A] => BVar[W, B] = ???

  implicit def BVarOrdering[A: Ordering, S]: Ordering[BVar[S, A]] = new Ordering[BVar[S, A]] {
    override def compare(x: BVar[S, A], y: BVar[S, A]): Int = implicitly[Ordering[A]].compare(x.v, y.v)
  }

  implicit def BVarEq[A: Equiv, S]: Equiv[BVar[S, A]] = new Equiv[BVar[S, A]] {
    override def equiv(x: BVar[S, A], y: BVar[S, A]): Boolean = implicitly[Equiv[A]].equiv(x.v, y.v)
  }
}