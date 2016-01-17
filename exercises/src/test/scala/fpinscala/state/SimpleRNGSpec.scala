package fpinscala.state

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SimpleRNGSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicit val RNGGen: Arbitrary[RNG] = Arbitrary(arbitrary[Long] map (n ⇒ RNG.Simple(n)))

  property("6.1 nonNegativeInt property") {
    forAll(arbitrary[RNG]) { rng ⇒
      val (n, rng2) = RNG.nonNegativeInt(rng)
      n should be >= 0
      RNG.nonNegativeInt(rng2)._1 should be >= 0
    }
  }

  property("6.2 double") {
    forAll(arbitrary[RNG]) { rng ⇒
      val (n, rng2) = RNG.double(rng)
      n.isInstanceOf[Double] should be (true)
      n should be >= 0.0
      n should be <  1.0
    }
  }
}
