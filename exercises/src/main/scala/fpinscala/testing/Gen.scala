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
  def unit[A](a: => A): Gen[A] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val nonNegativeInt: (RNG) ⇒ (Int, RNG) = RNG.nonNegativeInt
    val state: State[RNG, Int] = State(nonNegativeInt)
    Gen(state.map(n => start + n % (stopExclusive-start)))
  }

  // with implicit types, this is simplified to:
/*
  Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
*/
}

/*
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}
*/

trait SGen[+A] {

}

case class Gen[A](sample: State[RNG,A]) {
}
