package org.ad4s.core.op

sealed trait Op

case class Op1[T](run: T => (T, T => T)) extends Op
case class Op2[T](run: (T, T) => (T, T => (T, T))) extends Op
