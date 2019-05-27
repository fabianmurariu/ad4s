package org.ad4s.core.numeric

import org.ad4s.core.backprop.{Backprop, d}
import org.ad4s.core.op.{Op, Op1, Op2}
import spire.algebra.{Eq, Field, NRoot, Trig}
import spire.implicits._
import spire.math
import spire.math.{Algebraic, ConvertableFrom, ConvertableTo, Fractional, Rational, Real}

trait Math[@specialized(Float, Double) T] {

  implicit def F: Fractional[T]

  implicit def T: Trig[T]

  implicit def N: NRoot[T]

  implicit def B: Backprop[T]

  object ops {
    def plusOp: Op2[T, T, T] = Op.op2 { (a: T, b: T) => (a + b, { z => (z, z) }) }

    def minusOp: Op2[T, T, T] = Op.op2[T, T, T] { (a: T, b: T) => (a - b, { z => (z, -z) }) }

    def timesOp: Op2[T, T, T] = Op.op2[T, T, T] { (a: T, b: T) => (a * b, { z => (b * z, z * a) }) }

    def divOp: Op2[T, T, T] = Op.op2[T, T, T] { (a: T, b: T) => (a / b, { z => (z / b, (-z * a) / (b * b)) }) }

    def powOp(implicit N: NRoot[T], T: Trig[T]): Op2[T, T, T] = Op.op2[T, T, T] {
      (a: T, b: T) =>
        (N.fpow(a, b), { z =>
          val dx = b * N.fpow(a, b - 1)
          val dy = N.fpow(a, b * T.log(a))
          (z * dx, dy * z)
        })
    }


    def expOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.exp(a), { g => g * T.exp(a) }) }

    def logOp: Op1[T, T] = Op.op1[T, T] { a: T => (T.log(a), { g => g / a }) }

    def sigmoidOp: Op1[T, T] = Op.op1 {
      a: T =>
        val z = 1 / (T.exp(-a) + 1)
        (z, { g =>
          val ex = T.exp(-a)
          g * (ex / N.fpow(ex + 1, 2))
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

    def asinOp: Op1[T, T] = Op.op1 { x => (T.asin(x), { g => g / N.sqrt(1 - (x * x)) }) }

    def acosOp: Op1[T, T] = Op.op1 { x => (T.acos(x), { g => -(g / N.sqrt(1 - (x * x))) }) }

    def atanOp: Op1[T, T] = Op.op1 { x => (T.atan(x), { g => g / (1 + (x * x)) }) }

    def sinhOp: Op1[T, T] = Op.op1 { x => (T.sinh(x), { g => g * T.cosh(x) }) }

    def coshOp: Op1[T, T] = Op.op1 { x => (T.cosh(x), { g => g * T.sinh(x) }) }

    def tanhOp: Op1[T, T] = Op.op1 { x => (T.tanh(x), { g => g / T.cosh(x * x) }) }

    def atan2Op: Op2[T, T, T] = Op.op2 {
      (y, x) =>
        val z = T.atan2(y, x)
        (z, { g: T =>
          val common = (x * x) + (y * y)
          val dy = x / common
          val dx = -y / common
          (g * dy, g * dx)
        })
    }
  }

  def sigmoid(x: d[T]): d[T] =
    Op.liftOp1(x)(ops.sigmoidOp).unsafeRunSync()

  class BackpropAlgebra extends Fractional[d[T]] with Trig[d[T]] {

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

    override def fromType[B: ConvertableFrom](b: B): d[T] = d.const(F.fromType(b))

    override def nroot(a: d[T], n: Int): d[T] = ???

    override def fpow(a: d[T], b: d[T]): d[T] =
      Op.liftOp2(a, b)(ops.powOp).unsafeRunSync()

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

    override def toType[B](a: d[T])(implicit C: ConvertableTo[B]): B = F.toType(a.v)(C)

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

    def gcd(a: org.ad4s.core.backprop.d[T], b: org.ad4s.core.backprop.d[T])(implicit ev: spire.algebra.Eq[org.ad4s.core.backprop.d[T]]): org.ad4s.core.backprop.d[T] = ???

    def lcm(a: org.ad4s.core.backprop.d[T], b: org.ad4s.core.backprop.d[T])(implicit ev: spire.algebra.Eq[org.ad4s.core.backprop.d[T]]): org.ad4s.core.backprop.d[T] = ???

    override def e: d[T] = d.const(T.e)

    override def pi: d[T] = d.const(T.pi)

    override def exp(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.expOp).unsafeRunSync()

    override def expm1(a: d[T]): d[T] = ???

    override def log(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.expOp).unsafeRunSync()

    override def log1p(a: d[T]): d[T] = ???

    override def sin(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.sinOp).unsafeRunSync()

    override def cos(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.cosOp).unsafeRunSync()

    override def tan(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.tanOp).unsafeRunSync()

    override def asin(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.asinOp).unsafeRunSync()

    override def acos(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.acosOp).unsafeRunSync()

    override def atan(a: d[T]): d[T] =
      Op.liftOp1(a)(ops.atanOp).unsafeRunSync()

    override def atan2(y: d[T], x: d[T]): d[T] =
      Op.liftOp2(y, x)(ops.atan2Op).unsafeRunSync()

    override def sinh(x: d[T]): d[T] =
      Op.liftOp1(x)(ops.sinhOp).unsafeRunSync()

    override def cosh(x: d[T]): d[T] =
      Op.liftOp1(x)(ops.coshOp).unsafeRunSync()

    override def tanh(x: d[T]): d[T] =
      Op.liftOp1(x)(ops.tanhOp).unsafeRunSync()

    override def toRadians(a: d[T]): d[T] = ???

    override def toDegrees(a: d[T]): d[T] = ???

    override def gcd(a: d[T], b: d[T]): d[T] = ???
  }

  implicit def backpropAlgebra(implicit F: math.Fractional[T], T: Trig[T]): Fractional[d[T]] with Trig[d[T]] with Eq[d[T]] = new BackpropAlgebra()

}

object NumericOps extends Math