import ParserType._
import Parser._

object TestParser {

  def main(args: Array[String]): Unit = {

    assert("(())(())" == run(bracketParser.many)("(())(())").right.toOption.map(x => x.foldLeft("")((x, y) => {
      x + y.toString
    })).get)
  }


  case class Bracket(brackets: List[Bracket]) {

    override def toString : String =
      "(" + (brackets map(x => x.toString) mkString) + ")"
  }

  def bracketParser: Parser[Bracket] =
    for(_ <- char('(') ; b <- bracketParser.many; _ <- char(')')) yield Bracket(b)



}
