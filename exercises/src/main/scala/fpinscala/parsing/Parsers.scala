package fpinscala.parsing

import scala.language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A]) {


  }

  object Laws {
  }

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def succeed[A](a: A): Parser[A]


}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""


}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
