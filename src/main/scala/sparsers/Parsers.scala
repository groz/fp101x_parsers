package sparsers

import Parsing._

object Parsers {

  def result[A](a: A): Parser[A] = Parser { inp => (a, inp) :: Nil }

  def item: Parser[Char] = Parser {
    case Nil => Nil
    case c :: cs => (c, cs) :: Nil
  }

  def sat(p: Char => Boolean): Parser[Char] = item.filter(p)

  def char(x: Char): Parser[Char] = sat(_ == x)

  def digit: Parser[Char] = sat(_.isDigit)

  def lower: Parser[Char] = sat(_.isLower)

  def upper: Parser[Char] = sat(_.isUpper)

  def alphanum: Parser[Char] = sat(_.isLetterOrDigit)

  def letter: Parser[Char] = sat(_.isLetter)

  def string(str: Str): Parser[Str] = str match {
    case Nil => result(str)
    case x :: xs => for {
      _ <- char(x)
      _ <- string(xs)
    } yield x :: xs
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) +++ result(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      a <- p
      as <- many(p)
    } yield a :: as

  def whitespace: Parser[Char] = sat(_.isWhitespace)

  def spaces: Parser[Unit] = many1(whitespace).map(_ => ())

  def comment: Parser[Unit] =
    for {
      _ <- string("--")
      _ <- many(sat(_ != '\n'))
      _ <- char('\n')
    } yield ()A

  def nat: Parser[Int] = many1(digit).map(_.mkString.toInt)

  def int: Parser[Int] =
    (for {
      _ <- char('-')
      n <- nat
    } yield -n) +++ nat

  def junk: Parser[Unit] = many(spaces +++ comment).map(_ => ())

  def token[A](p: Parser[A]): Parser[A] =
    for {
      _ <- junk
      x <- p
      _ <- junk
    } yield x

  // parsers that skip junk and remove junk after
  def integer = token(int)

  def natural = token(nat)

  def symbol(s: Str) = token(string(s))
}
