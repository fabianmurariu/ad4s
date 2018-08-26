package org.ad4s.core.tape

import cats.effect.IO
import org.ad4s.core.{Bv, Ones, Sum}

import scala.collection.mutable.ArrayBuffer

private[tape] class Tape[T] {
  val nodes2: ArrayBuffer[Node[T]] = ArrayBuffer.empty[Node[T]]

  def len: Int = nodes2.length

  def reverse(n:Int = this.len): Seq[Int] = n to 0 by -1

  def insert0(tn: Node[T]): IO[Int] = IO {
    val n = len
    nodes2 += tn
    n
  }

}

object Tape {

  def runGrads[T](f : (Bv[T], Bv[T]) => BackpropContext[T] => Bv[T])
                 (t1: T, t2:T)(implicit O:Ones[T]):(T, T) = {
    val BC = new BackpropContext(new Tape[T])
    val bv1 = Bv(Idx(0), t1)
    val bv2 = Bv(Idx(1), t2)
    val out = f(bv1, bv2)(BC)

    val Grad(grads) = evalGrads(out, BC.tape)(O)
    (grads(0).asInstanceOf[T], grads(1).asInstanceOf[T])
  }

  def evalGrads[T](bv: Bv[T], t: Tape[T])(backprop: Ones[T]): Grad = {
    bv.i match {
      case InpRefN(i, _) =>
        val derivs = t.nodes2.map(_.zero)
        derivs(i) = backprop.ones(bv.v)
        for (j <- t.reverse(i)){
          val deriv = derivs(j)
          t.nodes2(j) match {
            case n:Node2[T] @unchecked =>
              val (k, l) = n.inputs
              val (wk, wl) = n.grad(deriv)
              k match {
                case idx:InpRefN[T] @unchecked=>
                  derivs(k.i) = idx.sum.add(derivs(idx.i), wk)
                case _ =>
              }
              l match {
                case idx:InpRefN[T] @unchecked =>
                  derivs(l.i) = idx.sum.add(derivs(idx.i), wl)
                case _ =>
              }
          }
        }
        Grad(derivs.toVector)
      case Idx(i) => ??? /* probably just a bunch of zeros*/
    }
  }
}

sealed trait InpRef{
  def i:Int
}

case class Idx(i:Int) extends InpRef
case class InpRefN[T](i: Int, sum: Sum[T]) extends InpRef

sealed trait Node[T] {
  def zero: T
}

case class Node2[T](inputs: (InpRef,InpRef), zero: T, grad: T => (T, T)) extends Node[T]

case class Grad(dxs: Vector[Any])

class BackpropContext[T](val tape: Tape[T]) {
  def insertNode(tn: Node[T]): IO[Int] =
    tape.insert0(tn)
}