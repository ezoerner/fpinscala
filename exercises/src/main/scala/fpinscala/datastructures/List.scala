package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(a,as) => a + sum(as) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(a,as) => a * product(as)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(a, Cons(2, Cons(4, _))) => a
    case Nil => 42
    case Cons(a, Cons(y, Cons(3, Cons(4, _)))) => a + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(a, xs) => f(a, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException
    case Cons(a, as) => Cons(h, as)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case m if m <= 0 => l
    case o if l == Nil => Nil
    case _ => drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case xs => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // implement using foldRight!
  def length[A](l: List[A]): Int = foldRight(l, 0)( (_, n) => n + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A])((acc, x) => Cons(x, acc))

  def reverseR[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) => appendElem(reverseR(t), h)
  }

  def appendElem[A](xs: List[A], x: A): List[A] =
    xs match {
      case Nil => Cons(x, Nil)
      case Cons(h, t) => Cons(h, appendElem(t, x))
    }

  def reverseFoldr[A](xs: List[A]): List[A] = foldRight(xs, List[A]()) { (a, as) =>
    appendElem(as, a)
  }

  // foldLeft in terms of foldRight
  def foldLeftFoldr[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverseFoldr(l), z){ (elm, acc) => f(acc, elm)}

  // foldRight in terms of foldLeft
  def foldRightFoldl[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z){ (acc, elm) => f(elm, acc)}

  // append in terms of foldLeft or foldRight
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // concatenate a list of lists into a single list using existing functions
  // must take time linear to the length of all lists
  def concat[A](lists: List[List[A]]): List[A] =
    foldRight(lists, List[A]()){ (list: List[A], cat: List[A]) =>
      foldRight(list, cat){ (a, as) =>
        Cons(a, as)
      }
    }
  // is the above implementation is not linear ot the length of all lists?
  // I believe so, but the answers have the following implementation (reusing the append function)
  def concatA[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil:List[A])(append)

  def addOne(list: List[Int]): List[Int] =
    foldRight(list, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def toStrings(list: List[Double]): List[String] =
    foldRight(list, Nil:List[String])((h,t) => Cons(h.toString, t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((h,t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((h,t) => append(t, f(h)))

  // from the answers
  def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // implement filter with flatMap
  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // function that accepts two lists and constructs a new list by
  // adding corresponding elements.
  def addCorresponding(l1: List[Int], l2: List[Int]): List[Int] =
    l1 match {
      case Nil => Nil
      case Cons(h1, t1) => l2 match {
        case Nil => Nil
        case Cons(h2, t2) => Cons(h1 + h2, addCorresponding(t1, t2))
      }
    }

  // from the answers
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
    case _ => Nil
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1,t1), Cons(h2,t2)) =>
      if (h1 == h2) startsWith(t1, t2)
      else false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1,t1), s) =>
        startsWith(Cons(h1,t1), s) || hasSubsequence(t1, s)
    }
}
