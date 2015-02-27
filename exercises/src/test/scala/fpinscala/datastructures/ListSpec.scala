package fpinscala.datastructures

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{Matchers, WordSpec}

class ListSpec
  extends WordSpec with Matchers with TypeCheckedTripleEquals  {

  "List" should {
    "reverse a list with reverse function" in {
      val list = List(1, 2, 3)
      List.reverse(list) should === (List(3, 2, 1))
    }

    "reverse a list with reverseR function" in {
      val list = List(1, 2, 3)
      List.reverseR(list) should === (List(3, 2, 1))
    }

    "append an elem to a list with appendElem" in {
      val list = List(1, 2, 3)
      List.appendElem(list, 4) should === (List(1, 2, 3, 4))
    }

    "produce reversed list with reverseFoldr function" in {
      val list = List(1, 2, 3)
      List.reverseFoldr(list) should === (List(3, 2, 1))
    }

    "fold left with foldLeftFoldr" in {
      val list = List(1, 2, 3)
      val reversed = List.foldLeftFoldr(list, Nil:List[Int])((b, a) => Cons(a, b))
      reversed should === (List(3, 2, 1))
    }

    "fold right with foldRightFoldl" in {
      val list = List(1, 2, 3)
      val reversed = List.foldRightFoldl(list, Nil:List[Int])((a, as) =>
        List.appendElem(as, a))
      reversed should === (List(3, 2, 1))
    }

    "append with appendFold" in {
      List.appendFold(List(1,2,3), List(4,5,6)) should === (List(1,2,3,4,5,6))
    }

    "concatenate a list of lists into single list" in {
      List.concat(List(List(1,2,3), List(4,5,6), List(7,8,9))) should === (List(1,2,3,4,5,6,7,8,9))
    }

    "concatenate a list of lists into single list using append" in {
      List.concatA(List(List(1,2,3), List(4,5,6), List(7,8,9))) should === (List(1,2,3,4,5,6,7,8,9))
    }

    "hasSubsequence" in {
      val sup = List(1,2,3,4)
      List.hasSubsequence(sup, List(1,2)) should === (true)
      List.hasSubsequence(sup, List(2,3)) should === (true)
      List.hasSubsequence(sup, List(4)) should === (true)
      List.hasSubsequence(sup, List(5)) should === (false)
      List.hasSubsequence(sup, List(1,2,3,5)) should === (false)
    }
  }
}
