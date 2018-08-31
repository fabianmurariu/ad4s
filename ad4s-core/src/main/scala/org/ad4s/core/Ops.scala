package org.ad4s.core

import cats.effect.IO
import org.ad4s.core.tape.{BackpropContext, InpRef, Node}

object Ops {
   def sin_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(x.i),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g * M.cos(x.v)) })
    ).map(i => Bv(InpRef(i, K.plus), M.sin(x.v)))

   def exp_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(x.i),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g * M.exp(x.v)) })
    ).map(i => Bv(InpRef(i, K.plus), M.exp(x.v)))

   def log_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(x.i),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g / x.v) })
    ).map(i => Bv(InpRef(i, K.plus), M.log(x.v)))

  def sin[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    sin_(x).unsafeRunSync()

  def exp[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    exp_(x).unsafeRunSync()

  def log[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    log_(x).unsafeRunSync()

  def pow_[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(M.pow(a.v, b.v))
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T =>
            val da = b.v * M.pow(a.v, K.minus(b.v, K.fromInt(1)))
            val db = M.pow(a.v, b.v * M.log(a.v))
            Seq(da * g, db * g)
          }))
    } yield Bv(InpRef(i, K.plus), v)

  def pow[T](a: Bv[T], b: Bv[T])
            (implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]):Bv[T] =
    pow_(a, b).unsafeRunSync()

  def plus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v + b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(K.one * g, K.one * g) }))
    } yield Bv(InpRef(i, K.plus), v)

  def times[T](a: Bv[T], b: Bv[T])
              (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v * b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(a.i, b.i),
          zero = K.zero(v),
          grad = { g: T => Seq(b.v * g, a.v * g) }))
    } yield Bv(InpRef(i, K.plus), v)
}