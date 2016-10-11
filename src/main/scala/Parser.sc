import Parser._
import ParserType.Parser

//string parser
run("hi")("hi i am here")

//slice


case class Bracket(brackets: List[Bracket]) {

  override def toString : String =
    "(" + (brackets map(x => x.toString) mkString) + ")"
}

def bracketParser: Parser[Bracket] =
  for(_ <- char('(') ; b <- bracketParser.many; _ <- char(')')) yield Bracket(b)


"(())()" == run(bracketParser.many)("(())()").right.toOption.map(x => x.foldLeft("")((x, y) => {
  x + y.toString
})).get