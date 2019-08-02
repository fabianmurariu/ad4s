package org.ad4s.core.numeric

import org.ad4s.core.op.Op.AOp2
import org.ad4s.core.op.{Op, Op1, Op2}
import spire.implicits._
import spire.math._
import spire.algebra._

abstract class Ops[@specialized(Float, Double) T](implicit val F: Fractional[T], T: Trig[T]) {

  def plusOp: AOp2[T] = Op.op2 { (a: T, b: T) => (a + b, { z => (z, z) }) }

  def minusOp: AOp2[T] = Op.op2[T, T, T] { (a: T, b: T) => (a - b, { z => (z, -z) }) }

  def timesOp: AOp2[T] = Op.op2[T, T, T] { (a: T, b: T) => (a * b, { z => (b * z, z * a) }) }

  def divOp: AOp2[T] = Op.op2[T, T, T] { (a: T, b: T) => (a / b, { z => (z / b, (-z * a) / (b * b)) }) }

  def powOp: Op2[T, T, T] = Op.op2[T, T, T] {
    (a: T, b: T) =>
      (F.fpow(a, b), { z =>
        val dx = b * F.fpow(a, b - 1)
        val dy = F.fpow(a, b * T.log(a))
        (z * dx, dy * z)
      })
  }

  def sqrtOp: Op1[T, T] = Op.op1[T, T] {
    (x: T) =>
      (F.sqrt(x), { z =>
        F.div(z, (2 * F.sqrt(x)))
      })
  }

  def expOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.exp(a), { g => g * T.exp(a) }) }

  def logOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.log(a), { g => g / a }) }

  def sigmoidOp: Op1[T, T] = Op.op1 {
    a: T =>
      val z = 1 / (T.exp(-a) + 1)
      (z, { g =>
        val ex = T.exp(-a)
        g * (ex / F.fpow(ex + 1, 2))
      })
  }

  def signumOp: Op1[T, T] = (v1: T) => (F.signum(v1), { _ => F.fromInt(0) })

  def negateOp: Op1[T, T] = (v1: T) => (F.negate(v1), F.negate)

  def absOp: Op1[T, T] = (v1: T) => (F.abs(v1), { g => g * F.signum(v1) })

  def ceilOp: Op1[T, T] = (v1: T) => (F.ceil(v1), { _ => F.fromInt(0) })

  def floorOp: Op1[T, T] = (v1: T) => (F.floor(v1), { _ => F.fromInt(0) })

  def roundOp: Op1[T, T] = (v1: T) => (F.round(v1), { _ => F.fromInt(0) })

  // Trig
  def sinOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.sin(a), { g => g * T.cos(a) }) }

  def cosOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.cos(a), { g => g * (-T.sin(a)) }) }

  def tanOp: Op1[T, T] = Op.op1[T, T] { x => (T.tan(x), { g => g / T.cos(x * x) }) }

  def asinOp: Op1[T, T] = Op.op1 { x => (T.asin(x), { g => g / F.sqrt(1 - (x * x)) }) }

  def acosOp: Op1[T, T] = Op.op1 { x => (T.acos(x), { g => -(g / F.sqrt(1 - (x * x))) }) }

  def atanOp: Op1[T, T] = Op.op1 { x => (T.atan(x), { g => g / (1 + (x * x)) }) }

  def sinhOp: Op1[T, T] = Op.op1 { x => (T.sinh(x), { g => g * T.cosh(x) }) }

  def coshOp: Op1[T, T] = Op.op1 { x => (T.cosh(x), { g => g * T.sinh(x) }) }

  def tanhOp: Op1[T, T] = Op.op1 { x => (T.tanh(x), { g => g / T.cosh(x * x) }) }

  def atan2Op: Op2[T, T, T] = Op.op2 {
    (y, x) =>
      val z = atan2(y, x)
      (z, { g: T =>
        val common = (x * x) + (y * y)
        val dy = x / common
        val dx = -y / common
        (g * dy, g * dx)
      })
  }
}

