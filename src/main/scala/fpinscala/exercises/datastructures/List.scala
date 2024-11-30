package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil        => sys.error("tail on empty list")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil        => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] = l match
    case _ if n <= 0 => l
    case Nil         => Nil
    case Cons(_, t)  => drop(t, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil                => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l

  def init1[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("init on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init1(t))

  def init[A](l: List[A]): List[A] = reverse(tail(reverse(l)))

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil        => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (acc, a) => acc + a)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, (acc, a) => acc * a)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (b, a) => Cons(a, b))

  def foldRightViaFoldLeft[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = foldLeft(reverse(as), acc, (a, b) => f(b, a))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons.apply)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], append)

  def incrementEach(l: List[Int]): List[Int] = map(l, _ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l, _.toString)

  infix def map[A, B](l: List[A], f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRightViaFoldLeft(
      as,
      Nil: List[A],
      (a, acc) => if f(a) then Cons(a, acc) else acc
    )

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B], (a, acc) => append(f(a), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b, _ + _)

  def zipWith[A, B, C](l: List[A], r: List[B], f: (A, B) => C): List[C] =
    def go(as: List[A], bs: List[B], acc: List[C]): List[C] = (as, bs) match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(a, tailA), Cons(b, tailB)) =>
        go(tailA, tailB, Cons(f(a, b), acc))
    reverse(go(l, r, Nil: List[C]))

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t)                => hasSubsequence(t, sub)
