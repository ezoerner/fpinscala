package fpinscala.monoids

import fpinscala.monoids.Monoid._
import org.scalacheck.Arbitrary
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.{PropertyChecks, TableFor1}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with PropertyChecks with Matchers with TypeCheckedTripleEquals {

  def table[A](monoid: Monoid[A]) = Table("monoid", monoid, monoid.dual)

  def monoidLaws[A](monoids: TableFor1[Monoid[A]])(implicit arbA: Arbitrary[A]): Unit =
    forAll(monoids) { (m: Monoid[A]) ⇒
      monoidLaws(m)
    }

  def monoidLaws[A](m: Monoid[A])(implicit arbA: Arbitrary[A]): Unit = {
    import m._
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "String Addition Monoid"

  it should "follow the monoid laws" in {
    type A = String
    val monoid = stringMonoid
    monoidLaws(table(monoid))
  }

  behavior of "String List Concatenation Monoid"

  it should "follow the monoid laws" in {
    type A = List[String]
    val monoid = listMonoid[String]
    monoidLaws(table(monoid))
  }

  behavior of "Integer List Concatenation Monoid"

  it should "follow the monoid laws" in {
    type A = List[Int]
    val monoid = listMonoid[Int]
    monoidLaws(table(monoid))
  }

  behavior of "Integer Addition Monoid"


  it should "follow the monoid laws" in {
    type A = Int
    val monoid = intAddition
    monoidLaws(table(monoid))
  }

  behavior of "Integer Multiplication Monoid"

  it should "follow the monoid laws" in {
    type A = Int
    val monoid = intMultiplication
    monoidLaws(table(monoid))
  }

  behavior of "Boolean Or Monoid"

  it should "follow the monoid laws" in {
    type A = Boolean
    val monoid = booleanOr
    monoidLaws(table(monoid))
  }

  behavior of "Boolean And Monoid"

  it should "follow associative law in `op`" in {
    type A = Boolean
    val monoid = booleanAnd
    monoidLaws(table(monoid))
  }

  behavior of "Int Option Combination Monoid"

  it should "follow the monoid laws" in {
    type A = Option[Int]
    val monoid = optionMonoid[Int]
    monoidLaws(table(monoid))
  }

  behavior of "Int endoMonoid"

  // equals isn't implemented for functions
  ignore should "follow the monoid laws" in {
    type A = Int
    val monoid = endoMonoid[Int]
    monoidLaws(table(monoid))
  }
}
