package sparsers

object MainApp extends App {

  import Parsers._
  import Parsing._

  val input: String = "2 + 2 * 3"

  /*
  Non left-recursive grammar:
    <Exp> ::= <Term> <Add> |
              <Term> <Sub>

    <Add> ::= + <Exp> | ε

    <Sub> ::= - <Exp> | ε

    <Term> ::= <Factor> <Mult> |
               <Factor> <Div>

    <Mult> ::= * <Term> | ε

    <Div> ::= / <Term> | ε

    <Factor> ::= Integer | (<Exp>)
  */

  def expr: Parser[Int] = (for {
    t <- term
    y <- add
  } yield t + y) +++ (for {
    t <- term
    y <- add
  } yield t - y)

  def add = (for {
    _ <- char('+')
    x <- expr
  } yield x) +++ result(0)

  def sub = (for {
    _ <- char('-')
    x <- expr
  } yield x) +++ result(0)

  def term = (for {
    x <- factor
    y <- mult
  } yield x * y) +++ (for {
    x <- factor
    y <- div
  } yield x / y)

  def mult = (for {
    _ <- char('*')
    x <- expr
  } yield x) +++ result(1)

  def div = (for {
    _ <- char('/')
    x <- expr
  } yield x) +++ result(1)

  def factor = integer +++ (for {
    _ <- char('(')
    x <- expr
    _ <- char(')')
  } yield x)


  println(expr.parse("(2+2)*2"))
  println(expr.parse("2+2*2"))
}
