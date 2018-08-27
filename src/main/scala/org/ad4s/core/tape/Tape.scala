package org.ad4s.core.tape

import cats.effect.IO
import org.ad4s.core.{Backprop, Bv, Ones, Sum, Zeros}

import scala.collection.mutable.ArrayBuffer

private[tape] class Tape[T] {
  val nodes2: ArrayBuffer[Node[T]] = ArrayBuffer.empty[Node[T]]

  def len: Int = nodes2.length

  def reverse(n: Int = this.len): Seq[Int] = n to 0 by -1

  def insert0(tn: Node[T]): IO[Int] = IO {
    val n = len
    nodes2 += tn
    n
  }

}

object Tape {

  def runGrads[T](f: (Bv[T], Bv[T]) => BackpropContext[T] => Bv[T])
                 (t1: T, t2: T)(implicit B: Backprop[T]): (T, T) = {
    val BC = new BackpropContext(new Tape[T])
    // move to init tape
    val n1 = Node(inputs = Seq.empty,
      zero = B.zeros(t1), grad = { t: T => Seq(B.zeros(t), B.zeros(t)) })
    val id1 = BC.insertNode(n1).unsafeRunSync()
    val bv1 = Bv(InpRefN(id1, B), t1)

    val n2 = Node(inputs = Seq.empty,
      zero = B.zeros(t2), grad = { t: T => Seq(B.zeros(t), B.zeros(t)) })
    val id2 = BC.insertNode(n2).unsafeRunSync()
    val bv2 = Bv(InpRefN(id2, B), t2)

    val out = f(bv1, bv2)(BC)

    val Grad(grads) = evalGrads(out, BC.tape)(B)
    (grads(0).asInstanceOf[T], grads(1).asInstanceOf[T])
  }

  def evalGrads[T](bv: Bv[T], t: Tape[T])(backprop: Ones[T]): Grad = {
    bv.i match {
      case InpRefN(i, _) =>
        val derivs = t.nodes2.map(_.zero)
        derivs(i) = backprop.ones(bv.v)
        for (j <- t.reverse(i)) {
          val deriv = derivs(j)
          t.nodes2(j) match {
            case n: Node[T]@unchecked =>
              val gradWeights = n.grad(deriv)
              n.inputs.zip(gradWeights).foreach {
                case (idx: InpRefN[T]@unchecked, w) =>
                  derivs(idx.i) = idx.sum.add(derivs(idx.i), w)
              }
          }
        }
        Grad(derivs.toVector)
    }
  }
}

sealed trait InpRef {
  def i: Int
}

case class InpRefN[T](i: Int, sum: Sum[T]) extends InpRef

case class Node[T](inputs: Seq[InpRef], zero: T, grad: T => Seq[T])

case class Grad(dxs: Vector[Any])

class BackpropContext[T](val tape: Tape[T]) {
  def insertNode(tn: Node[T]): IO[Int] =
    tape.insert0(tn)
}

object BackpropContext {

  object Implicits {
    implicit def backpropFromNumeric[T](implicit N: Fractional[T]): Backprop[T] = new Backprop[T] {
      override def ones(a: T): T = N.one

      override def add(a1: T, a2: T): T = N.plus(a1, a2)

      override def zeros(a: T): T = N.zero
    }
  }

}