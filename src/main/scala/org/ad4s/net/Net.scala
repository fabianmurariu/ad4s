package org.ad4s.net

import cats.Foldable

/**
  * Attempt to define the abstraction
  * that, when folding over a Foldable
  * will back-propagate the changes to its
  * weights given each example
  */
object Net {

  def runTrain[F[_], Net[_, _], A, B](n: Net[A, B])(data: F[(A, B)])
                                     (implicit Fold: Foldable[F],
                                      apNet: ApplyNet[Net, A, B],
                                      bp:BackPropagate[Net, A, B]): Net[A, B] =
    Fold.foldLeft(data, n) {
      case (net, (a, b_actual)) =>
        // basically SGD
        // 1. apply the loss function to 'a' and obtain the gradients
        // 2. apply the gradients to the net, thus changing the weights
        // 3. return the new net
        val (cost, gradFn) = apNet.runNet(net, a)
        val grads = gradFn(cost)
        bp.backProp(net, grads)
    }
}

trait BackPropagate[Net[_, _], X, Y] {
  def backProp(n: Net[X, Y], x: X): Net[X, Y]
}

trait ApplyNet[Net[_, _], X, Y] {
  def runNet(n: Net[X, Y], x: X): (Y, Y => X)
}
