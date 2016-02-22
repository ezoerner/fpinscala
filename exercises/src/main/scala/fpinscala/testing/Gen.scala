package fpinscala.testing

import fpinscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {

  def check: Boolean

  def &&(prop: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && prop.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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
}

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG,A]) {
  /** Exercise 8.6
    *   Implement flatMap, and then use it to implement this more dynamic
    *   version of listOfN. Put flatMap and listOfN in the Gen class.
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a ⇒ f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n ⇒
      Gen.listOfN(n, this)
    }
}

/*
// This is the same as:
case class Gen[A](sample: Rand[A])
*/

