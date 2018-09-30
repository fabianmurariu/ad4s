package org.ad4s.core.tape

import cats.effect.IO
import org.ad4s.core.backprop._

import scala.collection.mutable.ArrayBuffer

private[tape] class Tape {
  val nodes2: ArrayBuffer[TapeNode[_ <: Any]] = ArrayBuffer.empty[TapeNode[_ <: Any]]

  def len: Int = nodes2.length

  def reverse(n: Int = this.len): Seq[Int] = n to 0 by -1

  private[tape] def insert0(tn: TapeNode[_ <: Any]): IO[Int] = IO {
    val n = len
    nodes2 += tn
    n
  }

}

object Tape {

  def runGrads[X, Z](m: TapeEvaluatorMagnet[X, Z])
                    (x: X)(implicit B: Backprop[Z]): (Z, m.Grads) = {
    implicit val BC: BackpropContext = new BackpropContext(new Tape)

    val (out, gradBuilder) = m.eval(x)
    val grad = evalGrads(out, BC.tape)(B)
    (out.v, gradBuilder(grad))
  }

  def evalGrads[T](bv: d[T], t: Tape)(backprop: Ones[T]): Grad = {
    val derivs = t.nodes2.map(_.zero) // this may be wrong
    if (bv.i == DConst) Grad(derivs.toVector)
    else {
      val DRef(di, _) = bv.i
      derivs(di) = backprop.ones(bv.v)
      for (j <- t.reverse(di)) {
        val deriv = derivs(j)
        t.nodes2(j) match {
          case _: Node0[Any]@unchecked => /*do nothing*/
          case n1: Node1[Any, Any]@unchecked =>
            val w = n1.grad(deriv)
            derivs(n1.inp1.i) = n1.inp1.sum.add(derivs(n1.inp1.i), w)
          case n2: Node2[Any, Any, Any]@unchecked =>
            val (w1, w2) = n2.grad(deriv)
            derivs(n2.inp1.i) = n2.inp1.sum.add(derivs(n2.inp1.i), w1)
            derivs(n2.inp2.i) = n2.inp2.sum.add(derivs(n2.inp2.i), w2)
        }
      }
      Grad(derivs.toVector)
    }
  }
}

case class InpRef[T](i: Int, sum: Sum[T])

/* sum type might need extension */

sealed trait TapeNode[Z] {
  def zero: Z
}

case class Node0[Z](zero: Z) extends TapeNode[Z]

case class Node1[A1, Z](inp1: InpRef[A1], zero: Z, grad: Z => A1) extends TapeNode[Z]

case class Node2[A1, A2, Z](inp1: InpRef[A1], inp2: InpRef[A2], zero: Z, grad: Z => (A1, A2)) extends TapeNode[Z]

case class Grad(dxs: Vector[Any])

class BackpropContext(private[core] val tape: Tape = new Tape) {
  private[core] def insertNode[T](tn: TapeNode[T]): IO[Int] =
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