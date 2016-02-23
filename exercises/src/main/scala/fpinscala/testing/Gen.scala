package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing.Prop._

import scala.language.{implicitConversions, postfixOps}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) ⇒ Result) {
  def &&(p: Prop): Prop = Prop { (maxSize, testCases, rng) ⇒
    run(maxSize, testCases, rng) match {
      case Passed ⇒ p.run(maxSize, testCases, rng)
      case result ⇒ result
    }
  }

  def ||(p: Prop): Prop = Prop { (maxSize, testCases, rng) ⇒
    run(maxSize, testCases, rng) match {
      case Falsified(msg, _) ⇒
        p.tag(msg).run(maxSize, testCases, rng)
      case result ⇒ result
    }
  }

  /** This is rather simplistic - in the event of failure, we simply prepend
   *  the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

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

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)
}

case class Gen[A](sample: State[RNG,A]) {
  /** Exercise 8.6
    *   Implement flatMap, and then use it to implement this more dynamic
    *   version of listOfN. Put flatMap and listOfN in the Gen class.
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample flatMap (a ⇒ f(a).sample))

  def map[B](f: A => B): Gen[B] = Gen(sample map f)

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)
  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  /** =Exercise 8.10=
    * Implement helper functions for converting Gen to SGen.
    * You can add this as a method on Gen.
    */
  def unsized: SGen[A] = SGen(_ ⇒ this)
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

  /** Exercise 8.12
    * Implement a listOf combinator that doesn’t accept an explicit size.
    * It should return an SGen instead of a Gen. The implementation should
    * generate lists of the requested size.
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n ⇒ g.listOfN(Gen.unit(n)))

  /** =Exercise 8.13=
    * Define listOf1 for generating nonempty lists, and then update your
    * specification of max to use this generator.
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n ⇒ g.listOfN(Gen.unit(n max 1)))

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))
}
