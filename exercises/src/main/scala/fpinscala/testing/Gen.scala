package fpinscala.testing

import fpinscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

import Prop._
case class Prop(run: (TestCases, RNG) ⇒ Result) {
  def &&(p: Prop): Prop = Prop { (testCases, rng) ⇒
    run(testCases, rng) match {
      case Passed ⇒ p.run(testCases, rng)
      case result ⇒ result
    }
  }

  def ||(p: Prop): Prop = Prop { (testCases, rng) ⇒
    run(testCases, rng) match {
      case Falsified(_, _) ⇒
        p.run(testCases, rng)
      case result ⇒ result
    }
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val listOfRandA: List[State[RNG,A]] = List.fill(n)(g.sample)
    val randListOfA: State[RNG, List[A]] = State.sequence(listOfRandA)
    Gen(randListOfA): Gen[List[A]]
  }
  /* can be simplified to:
  Gen(State.sequence(List.fill(n)(g.sample)))
  */

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val nonNegativeInt: (RNG) ⇒ (Int, RNG) = RNG.nonNegativeInt
    val state: State[RNG, Int] = State(nonNegativeInt)
    Gen(state.map(n => start + n % (stopExclusive-start)))
  }
  /* can be simplified to:
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  */

  /**
    * Exercise 8.7
    * Implement union, for combining two generators of the same type into one,
    * by pulling values from each generator with equal likelihood.
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (if (_) g1 else g2)

  /**
    * Exercise 8.8
    * Implement weighted, a version of union that accepts a weight for each Gen
    * and generates values from each Gen with probability proportional to its
    * weight.
    */
  def rawRand2Gen[A](rawRand: RNG ⇒ (A, RNG)): Gen[A] = Gen(State(rawRand))
  val double: Gen[Double] = rawRand2Gen(RNG.double)

  // assumes the doubles passed in are positive
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val totalWeight = g1._2 + g2._2
    val weight1 = g1._2 / totalWeight
    double flatMap (d ⇒ if (d <= weight1) g1._1 else g2._1)
  }
}

case class SGen[+A](forSize: Int ⇒ Gen[A])

case class Gen[A](sample: State[RNG,A]) {
  /** Exercise 8.6
    *   Implement flatMap, and then use it to implement this more dynamic
    *   version of listOfN. Put flatMap and listOfN in the Gen class.
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample flatMap (a ⇒ f(a).sample))

  def map[B](f: A => B): Gen[B] = Gen(sample map f)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n ⇒
      Gen.listOfN(n, this)
    }

  /** Exercise 8.10
    * Implement helper functions for converting Gen to SGen.
    * You can add this as a method on Gen.
    */
  def unsized: SGen[A] = SGen(_ ⇒ this)
}

/*
// This is the same as:
case class Gen[A](sample: Rand[A])
*/

