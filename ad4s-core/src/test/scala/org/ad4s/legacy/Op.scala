//package org.ad4s.legacy
//
//import shapeless._
//import shapeless.ops.hlist._
//
//trait Op[In <: HList, A] {
//  type Out <: HList
//  val runOpWith: In => (A, A => Out)
//}
//
//trait NumericExtra[T] {
//  def pow(a: T, b: T): T
//}
//
//object Op {
//
//  type Kernel[T] = Numeric[T]
//
//  object Zero extends Poly1 {
//    implicit def numCase[N: Numeric]: Case.Aux[N, N] = at(_ => implicitly[Numeric[N]].zero)
//  }
//
//  def map[L <: HList](l: L, f: Poly)(implicit mapper: Mapper[f.type, L]): mapper.Out = mapper(l)
//
//  private[ad4s] def op1PloyMap[In <: HList, A: Kernel](a: A, f: Poly1)
//                                                      (implicit M: Mapper[f.type, In]): Op[In, A] = new Op[In, A] {
//    type Out = Mapper[f.type, In]#Out
//    val runOpWith: In => (A, A => Out) = {
//      h: In =>
//        (a, { _ => h.map(f) })
//    }
//  }
//
//  // can't seem to make this generic for any HList
//  def opConst[H <: HList, A: Kernel](x: A)(implicit M: Mapper[Zero.type, H]): Op[H, A] = op1PloyMap[H, A](x, Zero)
//
//  def op0[A: Kernel](x: A): Op[HNil, A] {type Out = HNil} = new Op[HNil, A] {
//    override type Out = HNil
//    override val runOpWith: HNil => (A, A => Out) = {
//      _ => (x, { _ => HNil })
//    }
//  }
//
//  def op1[A: Kernel, B: Kernel](f: A => (B, B => A)): Op[A :: HNil, B] {type Out = A :: HNil} = new Op[A :: HNil, B] {
//    type Out = A :: HNil
//    override val runOpWith: (A :: HNil) => (B, B => Out) = {
//      case a :: HNil =>
//        val (b, db) = f(a)
//        (b, { bb => db(bb) :: HNil })
//    }
//  }
//
//  def op2[A: Kernel, B: Kernel, C: Kernel](f: (A, B) => (C, C => (A, B))): Op[A :: B :: HNil, C] {type Out = A :: B :: HNil} = new Op[A :: B :: HNil, C] {
//    override type Out = A :: B :: HNil
//    override val runOpWith: A :: B :: HNil => (C, C => Out) = {
//      case a :: b :: HNil =>
//        val (c, dadb) = f(a, b)
//        (c, dadb andThen { ab: (A, B) => ab._1 :: ab._2 :: HNil })
//    }
//  }
//
//  def idOp[A: Kernel]: Op[A :: HNil, A] {
//    type Out = A :: HNil
//  } = op1[A, A](a => (a, identity))
//
//  // NOTE: in this API the multiplication from Tape happens in the Op, this is
//  // why Backprop only needs Add, Zero and One
//
//  def square[A](implicit K: Kernel[A]): Op[A :: HNil, A] {type Out = A :: HNil} =
//    op1(x => (K.times(x, x), d => K.times(K.fromInt(2), K.times(x, d))))
//
//  def plus[A](implicit K: Kernel[A]): Op[A :: A :: HNil, A] {type Out = A :: A :: HNil} =
//    op2[A, A, A]((x, y) => (K.plus(x, y), g => (g, g)))
//
//  def mul[A](implicit K: Kernel[A]): Op[A :: A :: HNil, A] {type Out = A :: A :: HNil} =
//    op2[A, A, A]((x, y) => (K.times(x, y), g => (K.times(g, y), K.times(g, x))))
//
////  def pow[A](implicit K: Kernel[A]): Op[A :: A :: HNil, A] {type Out = A :: A :: HNil} =
////    op2[A, A, A]((x, y) => (K.pow(x, y), g => {
////      val dx = y * x ^ (y -1)
////      val dy = x ^ y * log(x)
////    }))
//
//  def runOp[In <: HList, A: Kernel](op: Op[In, A])(as: In): (A, op.Out) = {
//    val (a, da) = op.runOpWith(as)
//    (a, da(implicitly[Kernel[A]].one))
//  }
//
//  class CombineOpHelper[A <: HList, B <: HList, C <: HList, N <: Nat, X: Numeric, Y: Numeric, Z: Numeric](op1: Op[A, X] {type Out = A}, op2: Op[B, Y] {type Out = B})
//                                                                                                         (op3: Op[X :: Y :: HNil, Z] {type Out = X :: Y :: HNil}) {
//    def apply()(implicit split: Split.Aux[C, N, A, B], P: Prepend[A, B]): Op[C, Z] {type Out = P.Out} = {
//      new Op[C, Z] {
//        override type Out = P.Out
//        override val runOpWith: C => (Z, Z => Out) = {
//          cs: C =>
//            // split the arguments
//            val (as, bs) = split(cs)
//            // run the left op and the right on the parts
//            val (x, gx) = op1.runOpWith(as)
//            val (y, gy) = op2.runOpWith(bs)
//            // run op3 on the combined outputs
//            val (z, gz) = op3.runOpWith(x :: y :: HNil)
//            z -> {
//              z0: Z =>
//                gz(z0) match {
//                  case x0 :: y0 :: HNil =>
//                    P.apply(gx.apply(x0), gy.apply(y0))
//                }
//            }
//        }
//      }
//    }
//  }
//
//}
