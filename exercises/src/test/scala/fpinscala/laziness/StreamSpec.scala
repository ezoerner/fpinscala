package fpinscala.laziness

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic.TypeCheckedTripleEquals._

import scala.annotation.tailrec

class StreamSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  def genStream(level: Int): Gen[Stream[Int]] =
    if (level >= 100)
      Gen.const(Empty)
    else
      Gen.oneOf(Gen.const(Empty), Gen.lzy(genNonEmpty(level + 1)))

  def genNonEmpty(level: Int): Gen[Stream[Int]] =
    for {
      h <- Arbitrary.arbitrary[Int]
      t <- genStream(level + 1)
    } yield Stream.cons(h, t)

  val streams: Gen[Stream[Int]] = genStream(0)


  property("stream toList is structurally equal to original") {
      @tailrec
      def streamEqualsList(strm: Stream[Int], list: List[Int]): Boolean =
        list match {
          case Nil => strm == Stream.empty
          case first :: rest =>
            strm.headOption forall (h => h == first)
            strm match {
              case Empty => false
              case Cons(h, t) => streamEqualsList(t(), rest)
            }
      }

    forAll(streams) { strm =>
      streamEqualsList(strm, strm.toList) should ===(true)
    }
  }

  property("drop(n) is equivalent to List.drop(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) =>
      (strm drop num).toList should === (strm.toList drop num)
    }
  }

  property("take(n) is equivalent to List.take(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) =>
      (strm take num).toList should === (strm.toList take num)
    }
  }

  property("takeWhile is equivalent to List.takeWhile") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm takeWhile p).toList should === (strm.toList takeWhile p)
    }
  }

  property("forAll is equivalent to List.forall") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm forAll p) should === (strm.toList forall p)
    }
  }

  property("takeWhileUsingFoldRight is equivalent to List.takeWhile") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm takeWhileUsingFoldRight p).toList should === (strm.toList takeWhile p)
    }
  }

  property("headOptionUsingFoldRight is equivalent to List.headOption") {
    forAll(streams) { strm =>
      strm.headOptionUsingFoldRight should === (strm.toList.headOption)
    }
  }

  property("map is equivalent to List.map") {
    forAll(streams) { strm =>
      val f: (Int => String) = _.toString
      (strm map f).toList should === (strm.toList map f)
    }
  }

  property("filter is equivalent to List.filter") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm filter p).toList should === (strm.toList filter p)
    }
  }

  property("append is equivalent to List.++") {
    forAll(streams, streams) { (strm1, strm2) =>
      (strm1 append strm2).toList should === (strm1.toList ++ strm2.toList)
    }
  }

  property("flatMap is equivalent to List.map") {
    forAll(streams) { strm =>
      val fStream: (Int => Stream[String]) = n => Stream.cons(n.toString, Stream.empty)
      val fList: (Int => List[String]) = n => n.toString :: List.empty
      (strm flatMap fStream).toList should === (strm.toList flatMap fList)
    }
  }
}
