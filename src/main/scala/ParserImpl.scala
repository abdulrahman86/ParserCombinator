
import ParserType.{Failure, Parser, Success}
import Parsers._

import scala.util.matching.Regex

object ParserType {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object Parser extends Parsers[Parser] {

  implicit def stringToLocation(s: String): Location = Location(s)

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(input) match {
    case Success(a, _) => Right(a)
    case Failure(x) => Left(x)
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  override def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

  override implicit def string(s: String): Parser[String] =
    input => {
      if (input.input.startsWith(s)) Success(s, s.length)
      else Failure(input.toError("Expected:" + s))
    }

  override def errorMessage(e: ParseError): String = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override implicit def regex(r: Regex): Parser[String] = ???

  override def slice[A](p: Parser[A]): Parser[String] =
    (input) => {
      p(input) match {
        case Success(_, x) => Success(input.input.substring(0, x), x)
        case x: Failure => x
      }
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def wrap[A](p: =>Parser[A]): Parser[A] = ???

  override def attempt[A](parser: Parser[A]): Parser[A] = ???
}