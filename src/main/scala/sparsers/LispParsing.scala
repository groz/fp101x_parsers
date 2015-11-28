package sparsers

import Parsing._
import Parsers._
/*
Write a lisp-like interpreter that evaluates expressions in the following grammar:
 Expr := (add Expr Expr)
 Expr := (mult Expr Expr)
 Expr := (let Name Expr Expr)
 Expr := Name
 Expr := Integer
 Name := String
Examples:
  (add 5 1) = 6
  (mult 3 5) = 15
  (add (mult 5 3) (mult 2 3)) = 21
  (let x (add 1 2) (add x 3)) = 6
  (let x (add 1 2) (let x (add 1 3) (mult x 2))) = 8
*/

object LispParsing extends App {
  def expr(ctx: Map[Str, Int]): Parser[Int] = (for {
    _ <- char('(')
    _ <- string("add ")
    expr1 <- expr(ctx)
    _ <- char(' ')
    expr2 <- expr(ctx)
    _ <- char(')')
  } yield expr1 + expr2) +++
  (for {
    _ <- char('(')
    _ <- string("mult ")
    expr1 <- expr(ctx)
    _ <- char(' ')
    expr2 <- expr(ctx)
    _ <- char(')')
  } yield expr1 * expr2) +++
  (for {
    _ <- char('(')
    _ <- string("let ")
    name <- identifier
    _ <- char(' ')
    value <- expr(ctx)
    _ <- char(' ')
    e <- expr(ctx+(name -> value))
    _ <- char(')')
  } yield e)  +++
  (for{
      name <- identifier
  } yield ctx(name)) +++ int


  println(expr(Map.empty).parse("5"))
  println(expr(Map.empty).parse("(add 5 6)"))
  println(expr(Map.empty).parse("(mult 5 6)"))
  println(expr(Map.empty).parse("(add 5 (mult 3 3))"))
  println(expr(Map.empty).parse("(mult (add 1 2) (mult 3 3))"))
  println(expr(Map.empty).parse("(let x (mult 2 1) (mult x x))"))

}

