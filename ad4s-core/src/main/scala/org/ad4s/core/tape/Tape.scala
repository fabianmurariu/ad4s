package org.ad4s.core.tape

import cats.effect.IO
import org.ad4s.core.{Backprop, Bv, Ones, Sum}

import scala.collection.mutable.ArrayBuffer

private[tape] class Tape {
  val nodes2: ArrayBuffer[Node[_ <: Any]] = ArrayBuffer.empty[Node[_ <: Any]]

  def len: Int = nodes2.length

  def reverse(n: Int = this.len): Seq[Int] = n to 0 by -1

  private[tape] def insert0(tn: Node[_ <: Any]): IO[Int] = IO {
    val n = len
    nodes2 += tn
    n
  }

}

object Tape {

  def runGrads[X, Z](m: TapeEvaluatorMagnet[X, Z])
                    (x: X)(implicit B: Backprop[Z]): (Z, m.Grads) = {
    implicit val BC: BackpropContext[Z] = new BackpropContext(new Tape)

    val (out, gradBuilder) = m.eval(x)
    val grad = evalGrads(out, BC.tape)(B)
    (out.v, gradBuilder(grad))
  }

  def evalGrads[T](bv: Bv[T], t: Tape)(backprop: Ones[T]): Grad = {
    val derivs = t.nodes2.map(_.zero)
    derivs(bv.i) = backprop.ones(bv.v)
    for (j <- t.reverse(bv.i)) {
      val deriv = derivs(j)
      t.nodes2(j) match {
        case n: Node[Any] @unchecked=>
          val gradWeights = n.grad(deriv)
          n.inputs.zip(gradWeights).foreach {
            case (idx: InpRef[Any] @unchecked, w) =>
              derivs(idx.i) = idx.sum.add(derivs(idx.i), w)
          }
      }
    }
    Grad(derivs.toVector)
  }
}

case class InpRef[T](i: Int, sum: Sum[T])

case class Node[T](inputs: Seq[InpRef[T]], zero: T, grad: T => Seq[T])

case class Grad(dxs: Vector[Any])

class BackpropContext[T](private[core] val tape: Tape = new Tape) {
  private[core] def insertNode(tn: Node[T]): IO[Int] =
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