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
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  //noinspection NotImplementedCode
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
