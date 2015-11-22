package sparsers

import scala.collection.immutable.StringOps

object Parsing {
  type Str = List[Char]
  type ParserFunc[A] = Str => List[(A, Str)]
  implicit def str2list(str: String): Str = new StringOps(str).toList
}
