package fpinscala.monoids

import fpinscala.monoids.Monoid._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with PropertyChecks with Matchers with TypeCheckedTripleEquals {

  behavior of "String Addition Monoid"

  it should "follow associative law in `op`" in {
    type A = String
    val monoid = stringMonoid

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = String
    val monoid = stringMonoid

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "String List Concatenation Monoid"

  it should "follow associative law in `op`" in {
    type A = List[String]
    val monoid = listMonoid[String]

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = List[String]
    val monoid = listMonoid[String]

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "Integer List Concatenation Monoid"

  it should "follow associative law in `op`" in {
    type A = List[Int]
    val monoid = listMonoid[Int]

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = List[Int]
    val monoid = listMonoid[Int]

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "Integer Addition Monoid"


  it should "follow associative law in `op`" in {
    type A = Int
    val monoid = intAddition

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = Int
    val monoid = intAddition

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "Integer Multiplication Monoid"

  it should "follow associative law in `op`" in {
    type A = Int
    val monoid = intMultiplication

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = Int
    val monoid = intMultiplication

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "Boolean Or Monoid"

  it should "follow associative law in `op`" in {
    type A = Boolean
    val monoid = booleanOr

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = Boolean
    val monoid = booleanOr

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }

  behavior of "Boolean And Monoid"

  it should "follow associative law in `op`" in {
    type A = Boolean
    val monoid = booleanAnd

    val op = monoid.op _
    forAll { (x: A, y: A, z: A) ⇒
      op(op(x, y), z) should === (op(x, op(y, z)))
    }
  }

  it should "follow identity law in 'zero'" in {
    type A = Boolean
    val monoid = booleanAnd

    val op = monoid.op _
    val zero = monoid.zero

    forAll { (x: A, y: A, z: A) ⇒
      op(x, zero) should === (x)
      op(zero, x) should === (x)
    }
  }
}
