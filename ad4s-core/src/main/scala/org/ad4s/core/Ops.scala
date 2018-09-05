package org.ad4s.core

import cats.effect.IO
import org.ad4s.core.tape.{BackpropContext, InpRef, Node}

object Ops extends LowPriorityOps

trait LowPriorityOps {
  def sigmoid_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(InpRef(x.i, K.plus)),
      zero = K.zero(x.v),
      grad = { g: T =>
        val ex = M.exp(-x.v)
        val dx = ex / M.powi(ex + K.fromInt(x.v, 1), 2)
        Seq(g * dx) })
    ).map(i => Bv(i, M.sigmoid(x.v)))

  def sin_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(InpRef(x.i, K.plus)),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g * M.cos(x.v)) })
    ).map(i => Bv(i, M.sin(x.v)))

  def exp_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(InpRef(x.i, K.plus)),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g * M.exp(x.v)) })
    ).map(i => Bv(i, M.exp(x.v)))

  def log_[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    B.insertNode(Node[T](
      inputs = Seq(InpRef(x.i, K.plus)),
      zero = K.zero(x.v),
      grad = { g: T => Seq(g / x.v) })
    ).map(i => Bv(i, M.log(x.v)))

  def sin[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    sin_(x).unsafeRunSync()

  def exp[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    exp_(x).unsafeRunSync()

  def log[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    log_(x).unsafeRunSync()

  def sigmoid[T](x: Bv[T])(implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    sigmoid_(x).unsafeRunSync()

  def pow_[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(M.pow(a.v, b.v))
      i <- B.insertNode(
        Node[T](
          inputs = Seq(InpRef(a.i, K.plus), InpRef(b.i, K.plus)),
          zero = K.zero(v),
          grad = { g: T =>
            val da = b.v * M.pow(a.v, K.minus(b.v, K.fromInt(b.v, 1)))
            val db = M.pow(a.v, b.v * M.log(a.v))
            Seq(da * g, db * g)
          }))
    } yield Bv(i, v)

  def pow[T](a: Bv[T], b: Bv[T])
            (implicit B: BackpropContext[T], K: Kernel[T], M: Maths[T]): Bv[T] =
    pow_(a, b).unsafeRunSync()

  def plus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v + b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(InpRef(a.i, K.plus), InpRef(b.i, K.plus)),
          zero = K.zero(v),
          grad = { g: T => Seq(g, g) }))
    } yield Bv(i, v)

  def minus[T](a: Bv[T], b: Bv[T])
             (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v - b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(InpRef(a.i, K.plus), InpRef(b.i, K.plus)),
          zero = K.zero(v),
          grad = { g: T => Seq(g, -g) }))
    } yield Bv(i, v)

  def times[T](a: Bv[T], b: Bv[T])
              (implicit B: BackpropContext[T], K: Kernel[T]): IO[Bv[T]] =
    for {
      v <- IO.pure(a.v * b.v)
      i <- B.insertNode(
        Node[T](
          inputs = Seq(InpRef(a.i, K.plus), InpRef(b.i, K.plus)),
          zero = K.zero(v),
          grad = { g: T => Seq(b.v * g, a.v * g) }))
    } yield Bv(i, v)
}