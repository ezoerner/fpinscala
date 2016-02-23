package fpinscala.laziness

import Stream._

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

  def takeWhileViaFoldRight(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else empty)

  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  def headOption: Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, t) ⇒ Some(h())
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, b) ⇒ Some(a))

  // 5.7 map, filter, append, flatmap Via foldRight. Part of the exercise is
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

  // Hint: Try to avoid using explicit recursion. Use `zipAll` and `takeWhile`.
  // TODO needs test
  def startsWith[B](s: Stream[B]): Boolean =
    (zipAll(s) takeWhile { case (_, bMaybe) ⇒ bMaybe.isDefined }).
      forAll { case (aMaybe, bMaybe) ⇒ aMaybe == bMaybe }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty             ⇒ None
    case strm @ Cons(_, t) ⇒ Some((strm, t()))
  } append Stream(empty)

  // TODO needs test
  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): Stream[B] = {

    case class Acc(scanResult: Stream[B], intermediate: B)

    foldRight(Acc(Stream(z), z)) { (a, acc) =>
      acc match {
        case Acc(scanResult, intermediate) =>
          val nextIntermediate: B = f(a, intermediate)
          val nextScanResult: Stream[B] = cons(nextIntermediate, scanResult)
          Acc(nextScanResult, nextIntermediate)
      }
    }.scanResult
  }

  def scanRightFromAnswer[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  // TODO needs test
  def mapViaUnfold[B](f: A ⇒ B): Stream[B] = unfold(this) {
    case Cons(h, t) ⇒ Some(f(h()), t())
    case _          ⇒ None
  }

  // TODO needs test
  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this, n) {
      case (Cons(h, t), i) if i > 0 ⇒ Some((h(), (t(), i - 1)))
      case _                        ⇒ None
    }

  // TODO needs test
  def takeWhileViaUnfold(p: A ⇒ Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) ⇒ Some((h(), t()))
      case _                    ⇒ None
    }

  // TODO needs test
  def zipWith[B, C](b: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    unfold((this, b)) {
      case (Cons(ha, ta), Cons(hb, tb)) ⇒ Some((f(ha(), hb()), (ta(), tb())))
      case _                            ⇒ None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  // TODO test this vs the version in the answers
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Empty, Empty) ⇒ None
      case (strmA, strmB) ⇒
        Some(
          ((strmA.headOption, strmB.headOption),
            (strmA.drop(1), strmB.drop(1))))
    }
  }
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
    cons(0, cons(1, rest(0, 1)))
  }

  val fibs = {
      def go(p0: Int, p1: Int): Stream[Long] = cons(p0, go(p1, p0 + p1))
    go(0, 1)
  }

  def unfold_1[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
    case None         ⇒ empty
  }

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z).fold(empty[A]) {
    case (a, s) ⇒ cons(a, unfold(s)(f))
  }

  val fibsViaUnfold = unfold((0, 1)) { case (p0, p1) ⇒ Some((p0, (p1, p0 + p1))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s ⇒ Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ ⇒ Some((a, a)))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ ⇒ Some((1, 1)))
}
