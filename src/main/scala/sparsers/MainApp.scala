package sparsers

object MainApp extends App {

  import Parsers._
  import Parsing._

  val input: String = "2 + 2 * 3"

  /*
  Non left-recursive grammar:
    <Exp> ::= <Term> <Add> | <Term> <Sub>
    <Add> ::= + <Exp> | ε
    <Sub> ::= - <Exp> | ε
    <Term> ::= <Factor> <Mult> | <Factor> <Div>
    <Mult> ::= * <Term> | ε
    <Div> ::= / <Term> | ε
    <Factor> ::= Integer | (<Exp>)
  */

  def expr: Parser[Expr] = for {
    t <- term
    x <- add(t) +++ sub(t) +++ result(t)
  } yield x

  def add(lhs: Expr) = for {
    _ <- char('+')
    x <- expr
  } yield Add(lhs, x)

  def sub(lhs: Expr) = for {
    _ <- char('-')
    x <- expr
  } yield Sub(lhs, x)

  def term: Parser[Expr] = for {
    f <- factor
    x <- mult(f) +++ div(f) +++ result(f)
  } yield x

  def mult(lhs: Expr) = for {
    _ <- char('*')
    x <- term
  } yield Mult(lhs, x)

  def div(lhs: Expr) = for {
    _ <- char('/')
    x <- term
  } yield Div(lhs, x)

  def factor = integer.map(Value) +++ (for {
    _ <- char('(')
    x <- expr
    _ <- char(')')
  } yield x)

  for {
    (e, out) <- expr.parse("2*3+4")
  } println(s"$e == ${e.eval}, unparsed: $out")
}
