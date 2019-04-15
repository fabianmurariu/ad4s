package org.ad4s.core.numeric

import org.ad4s.core.op.Ops._
import spire.algebra.{NRoot, Trig}
import spire.implicits._
import spire.math.Fractional

trait LowPriorityNumericOps {

  object ops {
    implicit def plusOp[T: Fractional]: Plus[T, T, T] = (a: T, b: T) => (a + b, { z => (z, z) })

    implicit def minusOp[T: Fractional]: Minus[T, T, T] = (a: T, b: T) => (a - b, { z => (z, -z) })

    implicit def timesOp[T: Fractional]: Times[T, T, T] = (a: T, b: T) => (a * b, { z => (b * z, z * a) })

    implicit def divOp[T: Fractional]: Div[T, T, T] = (a: T, b: T) => (a / b, { z => (z / b, (-z * a) / (b * b)) })

    implicit def powOp[T: Fractional](implicit N: NRoot[T], T: Trig[T]): Pow[T, T, T] = (a: T, b: T) => (N.fpow(a, b), { z =>
      val dx = b * N.fpow(a, b - 1)
      val dy = N.fpow(a, b * T.log(a))
      (z * dx, dy * z)
    })

    implicit def sinOp[T: Fractional](implicit T: Trig[T]): Sin[T, T] = (a: T) => (T.sin(a), { g => g * T.cos(a) })

    implicit def cosOp[T: Fractional](implicit T: Trig[T]): Cos[T, T] = (a: T) => (T.cos(a), { g => g * (-T.sin(a)) })

    implicit def expOp[T: Fractional](implicit T: Trig[T]): Exp[T, T] = (a: T) => (T.exp(a), { g => g * T.exp(a) })

    implicit def logOp[T: Fractional](implicit T: Trig[T]): Log[T, T] = (a: T) => (T.log(a), { g => g / a })

    implicit def sigmoidOp[T: Fractional](implicit T: Trig[T], N: NRoot[T]): Sigmoid[T, T] =
      (a: T) => {
        val z = 1 / (T.exp(-a) + 1)
        (z, { g =>
          val ex = T.exp(-a)
          g * (ex / N.fpow(ex + 1, 2))
        })
      }
  }

}

object NumericOps extends LowPriorityNumericOps