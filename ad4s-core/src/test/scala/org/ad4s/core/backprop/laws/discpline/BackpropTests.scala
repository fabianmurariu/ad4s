package org.ad4s.core.backprop.laws.discpline

import cats.Eq
import cats.kernel.laws.discipline._
import org.ad4s.core.backprop.Backprop
import org.ad4s.core.backprop.laws.BackpropLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait BackpropTests[A] extends Laws {

  def laws: BackpropLaws[A]

  def backprop(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "backprop", None,
      "identityLeft" -> forAll(laws.identityLeft _),
      "identityRight" -> forAll(laws.identityRight _),
      "commutativity" -> forAll(laws.commutativity _),
      "associativity" -> forAll(laws.associativity _),
      "idempotence" -> forAll(laws.idempotence _)
    )

}

object BackpropTests {
  def apply[A: Backprop]: BackpropTests[A] =
    new BackpropTests[A] {
      override def laws: BackpropLaws[A] = BackpropLaws[A]
    }
}
