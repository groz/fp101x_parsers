package sparsers

object ExprParsing extends App {

  import Parsers._
  import Parsing._

  /*
  Non left-recursive grammar:
    Expr ::= Term Add | Term Sub | Term
    Add ::= + Expr
    Sub ::= - Expr
    Term ::= Factor Mult | Factor Div | Factor
    Mult ::= * Term
    Div ::= / Term
    Factor ::= Integer | (Expr)
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
    (e, out) <- expr.parse("1+2*(3+4)-16/2")
  } println(s"$e == ${e.eval}, unparsed: $out")
}
