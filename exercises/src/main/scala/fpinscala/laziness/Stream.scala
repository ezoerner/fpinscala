package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {

  override def toString: String = toList.toString

  def toList: List[A] = this match {
    case Empty      ⇒ Nil
    case Cons(h, t) ⇒ h() :: t().toList
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          ⇒ z
    }

  def exists(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, b) ⇒ p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A ⇒ Boolean): Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, t) ⇒ if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if (n == 0)
      Empty
    else this match {
      case Empty      ⇒ Empty
      case Cons(h, t) ⇒ Cons(h, () ⇒ t() take (n - 1))
    }

  def drop(n: Int): Stream[A] =
    if (n == 0)
      this
    else this match {
      case Empty      ⇒ Empty
      case Cons(h, t) ⇒ t() drop (n - 1)
    }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) ⇒ Cons(h, () ⇒ t() takeWhile p)
    case _                    ⇒ Empty
  }

  def takeWhileUsingFoldRight(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else empty)

  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  def headOption: Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, t) ⇒ Some(h())
  }

  def headOptionUsingFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, b) ⇒ Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((elm, accum) ⇒ cons(f(elm), accum))

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A]) { (elm, accum) ⇒
      if (p(elm))
        cons(elm, accum)
      else
        accum
    }

  def append[B >: A](as: ⇒ Stream[B]): Stream[B] =
    foldRight(as)((elm, accum) ⇒ cons(elm, accum))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((elm, accum) ⇒ f(elm) append accum)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs_first_try: Stream[Int] = {
      def rest(prev1: Int, prev2: Int): Stream[Int] = {
        val next = prev1 + prev2
        cons(next, rest(prev2, next))
      }
    cons(0, cons(1, rest(0,1)))
  }

  val fibs = {
      def go(p0: Int, p1: Int): Stream[Long] = cons(p0, go(p1, p0 + p1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = sys.error("todo")
}
