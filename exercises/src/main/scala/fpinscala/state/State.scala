package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {

    /** random Int */
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // 6.1
  /** generate random Int from 0 to MaxValue, inclusive */
  def nonNegativeInt(rng: RNG): (Int,RNG) = {
    val (n, r) = rng.nextInt
    val resultN = if (n < 0) -(n + 1) else n
    (resultN, r)
  }

  // 6.2
  /** generate random double between 0 and 1, not including 1 */
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, rng2) ⇒ (n / (Int.MaxValue.toDouble + 1), rng2)
  }

  /**
    * =Exercise 6.3=
    * Write functions to generate an `(Int, Double)` pair, a `(Double, Int)` pair,
    * and a `(Double, Double, Double)` 3-tuple.
    * You should be able to reuse the functions you’ve already written.
    *  {{{
    *  def intDouble(rng: RNG): ((Int,Double), RNG)
    *  def doubleInt(rng: RNG): ((Double,Int), RNG)
    *  def double3(rng: RNG): ((Double,Double,Double), RNG)
    *  }}}
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r1) = intDouble(rng)
    ((d, i), r1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /** =Exercise 6.4=
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft((List.empty[Int], rng)) {
      case ((acc, r), _) ⇒
        val (nextI, nextR) = r.nextInt
        (nextI :: acc, nextR)
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * =Exercise 6.5=
    * Use map to reimplement double in a more elegant way. See exercise 6.2.
    */
  val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * =Exercise 6.6=
    * Write the implementation of `map2` based on the following signature.
    * This function takes two actions, `ra` and `rb`, and a function `f` for combining their results,
    * and returns a new action that combines them:
    * {{{
    *   def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
    * }}}
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng ⇒ {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  /**
    * =Exercise 6.7=
    * ''Hard:'' If you can combine two RNG transitions, you should be able to combine a whole list of them.
    * Implement `sequence` for combining a `List` of transitions into a single transition.
    * Use it to reimplement the `ints` function you wrote before.
    * For the latter, you can use the standard library function
    * `List.fill(n)(x)` to make a list with `x` repeated `n` times.
    * {{{
    * def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
    * }}}
    */
  def sequenceFirstAttempt[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng ⇒
        fs.foldRight( (List.empty[A], rng ) ) {
          case (nextRand, (accList, accRng)) ⇒
            val (a, rng) = nextRand(accRng)
            (a :: accList, rng)
        }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (nextRand, accList) ⇒ map2(nextRand, accList)(_ :: _) }

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
    * =Exercise 6.8=
    * Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
    */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThen(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i ⇒
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThen(n)
    }

  /** =Exercise 6.9=
    * Reimplement map and map2 in terms of flatMap
    */
  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a ⇒ unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))
}

/** =Exercise 6.10=
  * Generalize the functions `unit`, `map`, `map2`, `flatMap`, and `sequence`.
  * Add them as methods on the `State` case class where possible.
  * Otherwise you should put them in a `State` companion object.”
  */
object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s ⇒ (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    val z: State[S,List[A]] = unit(List.empty[A])
    fs.foldRight(z) { (nextState, accList) ⇒
      nextState.map2(accList)(_ :: _)
    }
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
import fpinscala.state.State._

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S,B] =
    flatMap(a ⇒ unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a ⇒ sb.map(b ⇒ f(a, b)))

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State { s =>
        val (a, s2) = run(s)
        f(a) run s2
      }
}


/**
  * =Exercise 6.11=
  * ''Hard:'' To gain experience with the use of `State`, implement a finite
  * state automaton that models a simple candy dispenser. The machine has two
  * types of input: you can insert a coin, or you can turn the knob to dispense
  * candy. It can be in one of two states: locked or unlocked. It also tracks
  * how many candies are left and how many coins it contains.
  *
  * The rules of the machine are as follows:
  * - Inserting a coin into a locked machine will cause it to unlock if there’s
  *   any candy left.
  * - Turning the knob on an unlocked machine will cause it to dispense candy
  *   and become locked.
  * - Turning the knob on a locked machine or inserting a coin into an unlocked
  *   machine does nothing.
  *
  * A machine that’s out of candy ignores all inputs. The method
  * `simulateMachine` should operate the machine based on the list of inputs and
  * return the number of coins and candies left in the machine at the end.
  * For example, if the input Machine has 10 coins and 5 candies, and a total
  * of 4 candies are successfully bought, the output should be (14, 1).
  */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


object Candy {
  // the following signature is the same as in the answer:
  //   def update = (i: Input) => (s: Machine) =>
  def update(i: Input)(s: Machine): Machine =
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      // the following is the same as in the answer:
      //     _ <- sequence(inputs map (modify[Machine] _ compose update))
      _ <- sequence(inputs map (i ⇒ modify[Machine](update(i))))
      s <- get
    } yield (s.coins, s.candies)
  }
}
