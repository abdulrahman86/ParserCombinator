
import ParserType.{Failure, Parser, Success}
import Parsers._

import scala.util.matching.Regex

object ParserType {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def advanceSuccess(n: Int) = this match {
      case Success(x, m) => Success(x, m + n)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]
}

object Parser extends Parsers[Parser] {

  implicit def stringToLocation(s: String): Location = Location(s)

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(input) match {
    case Success(a, _) => Right(a)
    case Failure(x, y) => Left(x)
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] =
    input => p(input) match {
      case Success(a, m) => {
        f(a)(input advanceBy m)advanceSuccess m
      }
      case e: Failure => e
    }

  override def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
    input => {
      p1 (input) match {
        case Failure(_, false) => p2(input)
        case x => x
      }
    }

  override implicit def string(s: String): Parser[String] =
    input => {
      if (input.input.substring(input.offset).startsWith(s)) Success(s, s.length)
      else Failure(input.toError("Expected:" + s))
    }

  override def errorMessage(e: ParseError): String = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    input => p(input) match {
      case Failure(x, y) => Failure((Location(input.input), msg) :: x.stack)
      case x => x
    }

  override def errorLocation(e: ParseError): Location = ???

  override implicit def regex(r: Regex): Parser[String] = ???

  override def slice[A](p: Parser[A]): Parser[String] =
    (input) => {
      p(input) match {
        case Success(_, x) => Success(input.input.substring(input.offset, x), x)
        case x: Failure => x
      }
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    input => {
      p(input) match {
        case Failure(x, y) => Failure(x.stack.lastOption.map(_._1).map((_, msg)).toList)
        case x => x
      }
    }

  override def wrap[A](p: =>Parser[A]): Parser[A] = input => p(input)

  override def attempt[A](parser: Parser[A]): Parser[A] =
    input => parser(input) match {
      case Failure(x, _) => Failure(x, false)
      case x => x
    }

  override def map[A, B](p: Parser[A])(f: (A) => B): Parser[B] =
    input => p(input) match {
      case Success(x, m) => Success(f(x), m)
      case e: Failure => e
    }
}