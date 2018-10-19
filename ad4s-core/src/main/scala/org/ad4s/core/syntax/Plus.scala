package org.ad4s.core.syntax

import org.ad4s.core.backprop.{Sum, Zeros, d}
import org.ad4s.core.op.{Op, Ops}
import simulacrum.{op, typeclass}

@typeclass trait Plus[A] {
  @op("|+|") def plus[B: Sum, C: Zeros](a: d[A], b: d[B])
                                       (implicit OP: Ops.Plus[A, B, C], SA: Sum[A]): d[C] =
    Op.liftOp2(a, b)(OP).unsafeRunSync()
  
}
