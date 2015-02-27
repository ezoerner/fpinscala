package fpinscala.gettingstarted

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{Matchers, WordSpec}

class GettingStartedSpec
  extends WordSpec with Matchers with TypeCheckedTripleEquals  {

  "Fibonacci series" should {
    import MyModule._

    "produce correct sequence" in {

      an [IllegalArgumentException] should be thrownBy fib(0)
      fib(1) should === (0)
      fib(2) should === (1)
      fib(3) should === (1)
      fib(4) should === (2)
      fib(5) should === (3)
      fib(6) should === (5)
    }
  }

  "isSorted" should {
    import PolymorphicFunctions._

    "check for sorted array" in {
      isSorted(Array(2,3,4), (_:Int) > (_: Int)) should === (true)
      isSorted(Array("b", "a"), (_:String) > (_: String)) should === (false)
      isSorted(Array("ab", "ba"), (_:String) > (_: String)) should === (true)
    }
  }
}
