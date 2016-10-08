import scala.util.matching.Regex

trait Parsers[Parser[+_]] {


  self =>

  import Parsers._

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString)map(_ charAt 0)
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def regex(r: Regex) : Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p:Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case n => map2(p, listOfN(n - 1, p))(_ :: _)
  }
  def many[A](p: Parser[A]): Parser[List[A]] = many1(p) or succeed(List())
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = for(a <- p) yield f(a)
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](p1: Parser[A], p2: Parser[B]) : Parser[(A,B)] = for(a <- p1; b <- p2)yield (a,b)
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2)map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(p.many))(_ :: _)
  def wrap[A](p: => Parser[A]): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def errorLocation(e: ParseError) : Location
  def errorMessage(e: ParseError) : String
  def label[A](msg: String)(p: Parser[A]) : Parser[A]
  def scope[A](msg: String)(p: Parser[A]) : Parser[A]
  def attempt[A](parser:Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def | [B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or [B >: A] (p2: => Parser[B]): Parser[B] = this | p2
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def many: Parser[List[A]]= self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice : Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]) : Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]) : Parser[(A,B)] = product[B](p2: Parser[B])
    def map2[B,C](p2: Parser[B])(f: (A,B) => C): Parser[C] =
      self.map2(p, p2)(f)
    def many1 : Parser[List[A]] = self.many1(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def label(msg: String) : Parser[A] = self.label(msg)(p)
    def scope(msg: String) : Parser[A] = self.scope(msg)(p)
    def attempt = self.attempt(p)
  }
}

object Parsers {

  case class ParseError(stack: List[(Location, String)])

  case class Location(input: String, offset: Int = 0){

    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(s: String) : ParseError = List((this, s))
  }

  implicit def toParseError(a: List[(Location, String)]): ParseError = ParseError(a)
}
