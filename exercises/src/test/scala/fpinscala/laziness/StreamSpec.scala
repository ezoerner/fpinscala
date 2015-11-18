package fpinscala.laziness

import fpinscala.gettingstarted.MyModule._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic.TypeCheckedTripleEquals._
import Arbitrary._

import scala.annotation.tailrec

class StreamSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  def genStream(level: Int): Gen[Stream[Int]] =
    if (level >= 100)
      Gen.const(Empty)
    else
      Gen.oneOf(Gen.const(Empty), Gen.lzy(genNonEmpty(level + 1)))

  def genNonEmpty(level: Int): Gen[Stream[Int]] =
    for {
      h ← arbitrary[Int]
      t ← genStream(level + 1)
    } yield Stream.cons(h, t)

  val streams: Gen[Stream[Int]] = genStream(0)

  property("stream toList is structurally equal to original") {
      @tailrec
      def streamEqualsList(strm: Stream[Int], list: List[Int]): Boolean =
        list match {
          case Nil ⇒ strm == Stream.empty
          case first :: rest ⇒
            strm.headOption forall (h ⇒ h == first)
            strm match {
              case Empty      ⇒ false
              case Cons(h, t) ⇒ streamEqualsList(t(), rest)
            }
        }

    forAll(streams) { strm ⇒
      streamEqualsList(strm, strm.toList) should ===(true)
    }
  }

  property("drop(n) is equivalent to List.drop(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) ⇒
      (strm drop num).toList should ===(strm.toList drop num)
    }
  }

  property("take(n) is equivalent to List.take(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) ⇒
      (strm take num).toList should ===(strm.toList take num)
    }
  }

  property("takeWhile is equivalent to List.takeWhile") {
    forAll(streams) { strm ⇒
      val p: (Int ⇒ Boolean) = _ % 2 == 0
      (strm takeWhile p).toList should ===(strm.toList takeWhile p)
    }
  }

  property("forAll is equivalent to List.forall") {
    forAll(streams) { strm ⇒
      val p: (Int ⇒ Boolean) = _ % 2 == 0
      (strm forAll p) should ===(strm.toList forall p)
    }
  }

  property("takeWhileViaFoldRight is equivalent to List.takeWhile") {
    forAll(streams) { strm ⇒
      val p: (Int ⇒ Boolean) = _ % 2 == 0
      (strm takeWhileViaFoldRight p).toList should ===(strm.toList takeWhile p)
    }
  }

  property("headOptionViaFoldRight is equivalent to List.headOption") {
    forAll(streams) { strm ⇒
      strm.headOptionViaFoldRight should ===(strm.toList.headOption)
    }
  }

  property("map is equivalent to List.map") {
    forAll(streams) { strm ⇒
      val f: (Int ⇒ String) = _.toString
      (strm map f).toList should ===(strm.toList map f)
    }
  }

  property("filter is equivalent to List.filter") {
    forAll(streams) { strm ⇒
      val p: (Int ⇒ Boolean) = _ % 2 == 0
      (strm filter p).toList should ===(strm.toList filter p)
    }
  }

  property("append is equivalent to List.++") {
    forAll(streams, streams) { (strm1, strm2) ⇒
      (strm1 append strm2).toList should ===(strm1.toList ++ strm2.toList)
    }
  }

  property("flatMap is equivalent to List.map") {
    forAll(streams) { strm ⇒
      val fStream: (Int ⇒ Stream[String]) = n ⇒ Stream.cons(n.toString, Stream.empty)
      val fList: (Int ⇒ List[String]) = n ⇒ n.toString :: List.empty
      (strm flatMap fStream).toList should ===(strm.toList flatMap fList)
    }
  }

  property("infinite stream of a constant") {
    forAll(Gen.choose(0, 100), arbitrary[Int]) { (take, const) ⇒
      Stream.constant(const).take(take).toList should ===(List.fill(take)(const))
    }
  }

  property("infinite stream counting up from") {
      @tailrec
      def check(countdown: Int, expected: Int, stream: Stream[Int]): Unit =
        if (countdown == 0)
          stream.headOption should ===(None)
        else {
          stream.headOption should ===(Some(expected))
          check(countdown - 1, expected + 1, stream match {
            case Empty      ⇒ fail("stream should not be empty")
            case Cons(h, t) ⇒ t()
          })
        }

    forAll(Gen.choose(0, 100), arbitrary[Int]) { (take, start) ⇒
      val list = Stream.from(start).take(take)
      check(take, start, list)
    }
  }

  property("verify up to 200 fibs") {
    forAll(Gen.choose(1, 200)) { n =>
      Stream.fibs.take(n).toList should === (((1 to n) map (i => fib(i))).toList)
    }
  }

  property("unfold n even numbers starting with zero") {
    // a function producing n even numbers starting with zero
    forAll(Gen.choose(0, 100)) { num =>
      val stream = Stream.unfold(0) { state =>
        if (state < num) {
          val nextState = state + 1
          Some((state * 2, nextState))
        }
        else
          None
      }

      stream.toList should === ((0 until num * 2 by 2).toList)
    }
  }

  property("verify up to 200 fibsViaUnfold") {
    forAll(Gen.choose(1, 200)) { n =>
      Stream.fibsViaUnfold.take(n).toList should === (((1 to n) map (i => fib(i))).toList)
    }
  }

  property("infinite stream counting up from Via unfold") {
    @tailrec
    def check(countdown: Int, expected: Int, stream: Stream[Int]): Unit =
      if (countdown == 0)
        stream.headOption should ===(None)
      else {
        stream.headOption should ===(Some(expected))
        check(countdown - 1, expected + 1, stream match {
          case Empty      ⇒ fail("stream should not be empty")
          case Cons(h, t) ⇒ t()
        })
      }

    forAll(Gen.choose(0, 100), arbitrary[Int]) { (take, start) ⇒
      val list = Stream.fromViaUnfold(start).take(take)
      check(take, start, list)
    }
  }

  property("infinite stream of a constant Via unfold") {
    forAll(Gen.choose(0, 100), arbitrary[Int]) { (take, const) ⇒
      Stream.constantViaUnfold(const).take(take).toList should ===(List.fill(take)(const))
    }
  }

  property("infinite stream of ones Via unfold") {
    forAll(Gen.choose(0, 100), arbitrary[Int]) { (take, const) ⇒
      Stream.onesViaUnfold.take(take).toList should ===(List.fill(take)(1))
    }
  }

  property("example tails") {
    val actual = Stream(1,2,3).tails.toList map (_.toList)
    val expected = List(List(1,2,3), List(2,3), List(3), Nil)
    actual should === (expected)
  }
}
