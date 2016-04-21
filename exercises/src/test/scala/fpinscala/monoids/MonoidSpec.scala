package fpinscala.monoids

import fpinscala.monoids.Monoid._
import org.scalacheck.Arbitrary
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.{PropertyChecks, TableFor1}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with PropertyChecks with Matchers with TypeCheckedTripleEquals {

  def table[A](monoid: Monoid[A]) = Table("monoid", monoid, monoid.dual)

  def assocLaw[A](monoids: TableFor1[Monoid[A]])(implicit arbA: Arbitrary[A]) =
    forAll(monoids) { (m: Monoid[A]) ⇒
      import m._
      forAll { (x: A, y: A, z: A) ⇒
        op(op(x, y), z) should ===(op(x, op(y, z)))
      }
    }

  def identityLaw[A](monoids: TableFor1[Monoid[A]])(implicit arbA: Arbitrary[A]) =
    forAll(monoids) { (m: Monoid[A]) ⇒
      import m._
      forAll { (x: A, y: A, z: A) ⇒
        op(x, zero) should === (x)
        op(zero, x) should === (x)
      }
    }

  behavior of "String Addition Monoid"

  it should "follow associative law in `op`" in {
    type A = String
    val monoid = stringMonoid

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = String
    val monoid = stringMonoid

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "String List Concatenation Monoid"

  it should "follow associative law in `op`" in {
    type A = List[String]
    val monoid = listMonoid[String]

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = List[String]
    val monoid = listMonoid[String]

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Integer List Concatenation Monoid"

  it should "follow associative law in `op`" in {
    type A = List[Int]
    val monoid = listMonoid[Int]

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = List[Int]
    val monoid = listMonoid[Int]

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Integer Addition Monoid"


  it should "follow associative law in `op`" in {
    type A = Int
    val monoid = intAddition

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = Int
    val monoid = intAddition

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Integer Multiplication Monoid"

  it should "follow associative law in `op`" in {
    type A = Int
    val monoid = intMultiplication

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = Int
    val monoid = intMultiplication

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Boolean Or Monoid"

  it should "follow associative law in `op`" in {
    type A = Boolean
    val monoid = booleanOr

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = Boolean
    val monoid = booleanOr

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Boolean And Monoid"

  it should "follow associative law in `op`" in {
    type A = Boolean
    val monoid = booleanAnd

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = Boolean
    val monoid = booleanAnd

    val monoids = table(monoid)
    identityLaw(monoids)
  }

  behavior of "Int Option Combination Monoid"

  it should "follow associative law in `op`" in {
    type A = Option[Int]
    val monoid = optionMonoid[Int]

    val monoids = table(monoid)
    assocLaw(monoids)
  }

  it should "follow identity law in 'zero'" in {
    type A = Option[Int]
    val monoid = optionMonoid[Int]

    val monoids = table(monoid)
    identityLaw(monoids)
  }
}
