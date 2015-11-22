package sparsers

import Parsing._

case class Parser[+A](p: ParserFunc[A]) {

  def parse(str: Str) = p(str)

  def map[B](f: A => B): Parser[B] = Parser { inp =>
    for {
      (a, out) <- parse(inp)
    } yield (f(a), out)
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { inp =>
    for {
      (a, out) <- parse(inp)
      bout <- f(a).parse(out)
    } yield bout
  }

  def filter(p: A => Boolean): Parser[A] = Parser { inp =>
    for {
      (a, out) <- parse(inp)
      if p(a)
    } yield (a, out)
  }

  def +++[B >: A](next: Parser[B]): Parser[B] = Parser { inp =>
    parse(inp) match {
      case Nil => next.parse(inp)
      case x => x
    }
  }
}

