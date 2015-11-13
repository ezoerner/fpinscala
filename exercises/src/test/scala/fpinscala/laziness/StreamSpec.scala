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
            streamEqualsList(strm.drop(1), rest)
        }

    forAll(streams) { strm =>
      streamEqualsList(strm, strm.toList) should ===(true)
    }
  }

  property("drop(n) mirrors List.drop(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) =>
      (strm drop num).toList should === (strm.toList drop num)
    }
  }

  property("take(n) mirrors List.take(n)") {
    forAll(streams, Gen.choose(0, 30)) { (strm, num) =>
      (strm take num).toList should === (strm.toList take num)
    }
  }

  property("takeWhile mirrors List.takeWhile") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm takeWhile p).toList should === (strm.toList takeWhile p)
    }
  }

  property("forAll mirrors List.forall") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm forAll p) should === (strm.toList forall p)
    }
  }

  property("takeWhileUsingFoldRight mirrors List.takeWhile") {
    forAll(streams) { strm =>
      val p: (Int => Boolean) = _ % 2 == 0
      (strm takeWhileUsingFoldRight p).toList should === (strm.toList takeWhile p)
    }
  }
}
