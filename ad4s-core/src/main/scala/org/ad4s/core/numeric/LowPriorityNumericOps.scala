package org.ad4s.core.numeric

import org.ad4s.core.backprop.{Backprop, d}
import org.ad4s.core.op.{Op, Op1}
import org.ad4s.core.op.Ops._
import spire.algebra.{NRoot, Trig}
import spire.implicits._
import spire.math
import spire.math.{Algebraic, ConvertableFrom, ConvertableTo, Fractional, Rational, Real}
import spire.syntax._

trait LowPriorityNumericOps {

  object ops {
    def plusOp[T: math.Fractional]: Plus[T, T, T] = (a: T, b: T) => (a + b, { z => (z, z) })

    def minusOp[T: math.Fractional]: Minus[T, T, T] = (a: T, b: T) => (a - b, { z => (z, -z) })

    def timesOp[T: math.Fractional]: Times[T, T, T] = (a: T, b: T) => (a * b, { z => (b * z, z * a) })

    def divOp[T: math.Fractional]: Div[T, T, T] = (a: T, b: T) => (a / b, { z => (z / b, (-z * a) / (b * b)) })

    def powOp[T: math.Fractional](implicit N: NRoot[T], T: Trig[T]): Pow[T, T, T] = (a: T, b: T) => (N.fpow(a, b), { z =>
      val dx = b * N.fpow(a, b - 1)
      val dy = N.fpow(a, b * T.log(a))
      (z * dx, dy * z)
    })

    def sinOp[T: math.Fractional](implicit T: Trig[T]): Sin[T, T] = (a: T) => (T.sin(a), { g => g * T.cos(a) })

    def cosOp[T: math.Fractional](implicit T: Trig[T]): Cos[T, T] = (a: T) => (T.cos(a), { g => g * (-T.sin(a)) })

    def expOp[T: math.Fractional](implicit T: Trig[T]): Exp[T, T] = (a: T) => (T.exp(a), { g => g * T.exp(a) })

    def logOp[T: math.Fractional](implicit T: Trig[T]): Log[T, T] = (a: T) => (T.log(a), { g => g / a })

    def sigmoidOp[T: Fractional](implicit T: Trig[T], N: NRoot[T]): Sigmoid[T, T] =
      (a: T) => {
        val z = 1 / (T.exp(-a) + 1)
        (z, { g =>
          val ex = T.exp(-a)
          g * (ex / N.fpow(ex + 1, 2))
        })
      }

    def signumOp[T](implicit F: math.Numeric[T]): Op1[T, T] = (v1: T) => (F.signum(v1), { _ => F.fromInt(0) })

    def negateOp[T](implicit F: math.Fractional[T]): Op1[T, T] = (v1: T) => (F.negate(v1), F.negate)

    def absOp[T](implicit F: math.Fractional[T]): Op1[T, T] = (v1: T) => (F.abs(v1), { g => g * F.signum(v1) })

    def ceilOp[T](implicit F: math.Fractional[T]): Op1[T, T] = (v1: T) => (F.ceil(v1), { _ => F.fromInt(0) })

    def floorOp[T](implicit F: math.Fractional[T]): Op1[T, T] = (v1: T) => (F.floor(v1), { _ => F.fromInt(0) })

    def roundOp[T](implicit F: math.Fractional[T]): Op1[T, T] = (v1: T) => (F.round(v1), { _ => F.fromInt(0) })

  }

  implicit def backpropAlgebra[T: Backprop](implicit F: math.Fractional[T]): Fractional[d[T]] = new math.Fractional[d[T]] {
    override def negate(x: d[T]): d[T] =
      Op.liftOp1(x)(ops.negateOp).unsafeRunSync()

    override def compare(x: d[T], y: d[T]): Int = F.compare(x.v, y.v)

    override def plus(x: d[T], y: d[T]): d[T] =
      Op.liftOp2(x, y)(ops.plusOp).unsafeRunSync()

    override def signum(a: d[T]): Int = F.signum(a.v)

    override def abs(a: d[T]): d[T] = Op.liftOp1(a)(ops.absOp).unsafeRunSync()

    override def fromByte(n: Byte): d[T] = d.const(F.fromByte(n))

    override def fromShort(n: Short): d[T] = d.const(F.fromShort(n))

    override def fromLong(n: Long): d[T] = d.const(F.fromLong(n))

    override def fromFloat(n: Float): d[T] = d.const(F.fromFloat(n))

    override def fromBigInt(n: BigInt): d[T] = d.const(F.fromBigInt(n))

    override def fromBigDecimal(n: BigDecimal): d[T] = d.const(F.fromBigDecimal(n))

    override def fromRational(n: Rational): d[T] = d.const(F.fromRational(n))

    override def fromAlgebraic(n: Algebraic): d[T] = d.const(F.fromAlgebraic(n))

    override def fromReal(n: Real): d[T] = d.const(F.fromReal(n))

    override def fromType[B](b: B)(implicit evidence$1: ConvertableFrom[B]): d[T] = d.const(F.fromType(b))

    override def nroot(a: d[T], n: Int): d[T] = ???

    override def fpow(a: d[T], b: d[T]): d[T] = ???

    override def one: d[T] = d.const(F.one)

    override def zero: d[T] = d.const(F.zero)

    override def toByte(a: d[T]): Byte = F.toByte(a.v)

    override def toShort(a: d[T]): Short = F.toShort(a.v)

    override def toInt(a: d[T]): Int = F.toInt(a.v)

    override def toLong(a: d[T]): Long = F.toLong(a.v)

    override def toFloat(a: d[T]): Float = F.toFloat(a.v)

    override def toBigInt(a: d[T]): BigInt = F.toBigInt(a.v)

    override def toBigDecimal(a: d[T]): BigDecimal = F.toBigDecimal(a.v)

    override def toRational(a: d[T]): Rational = F.toRational(a.v)

    override def toAlgebraic(a: d[T]): Algebraic = F.toAlgebraic(a.v)

    override def toNumber(a: d[T]): math.Number = F.toNumber(a.v)

    override def toType[B](a: d[T])(implicit evidence$17: ConvertableTo[B]): B = F.toType(a.v)

    override def toString(a: d[T]): String = a.toString

    override def div(x: d[T], y: d[T]): d[T] =
      Op.liftOp2(x, y)(ops.divOp).unsafeRunSync()

    override def ceil(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.ceilOp).unsafeRunSync()

    override def floor(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.floorOp).unsafeRunSync()

    override def round(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.roundOp).unsafeRunSync()

    override def isWhole(a: d[T]): Boolean = F.isWhole(a.v)

    override def toDouble(a: d[T]): Double = F.toDouble(a.v)

    override def toReal(a: d[T]): Real = F.toReal(a.v)

    override def times(x: d[T], y: d[T]): d[T] =
      Op.liftOp2(x, y)(ops.timesOp).unsafeRunSync()

    override def fromDouble(n: Double): d[T] = d.const(F.fromDouble(n))

    override def quot(a: d[T], b: d[T]): d[T] = ???

    override def mod(a: d[T], b: d[T]): d[T] = ???

    override def gcd(a: d[T], b: d[T]): d[T] = ???
  }

}

object NumericOps extends LowPriorityNumericOps