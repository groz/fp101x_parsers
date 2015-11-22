package sparsers

import Parsing._
import Parsers._

object JsonParsing extends App {

  // todo: handle whitespaces

  def jsonValue: Parser[JsonValue] = jsonNull +++ jsonBool +++ jsonNum +++ jsonStr +++ jsonObj +++ jsonArr

  val jsonNull = string("null").map(_ => JsonNull)

  val jsonBool = (string("true") +++ string("false")).map(_.mkString.toBoolean).map(JsonBoolean)

  // todo: implement for Double
  def jsonNum: Parser[JsonNumber] = integer.map(JsonNumber(_))

  // todo: implement proper standard (escape symbols, etc)
  def jsonStr: Parser[JsonString] = for {
    _ <- char('"')
    s <- many(sat(_ != '"'))
    _ <- char('"')
  } yield JsonString(s.mkString)

  def jsonObjItem = for {
    k <- jsonStr
    _ <- char(':')
    v <- jsonValue
  } yield k -> v

  def jsonObjNextItem = for {
    _ <- char(',')
    i <- jsonObjItem
  } yield i

  def jsonEmptyObj: Parser[JsonObject] = for {
    _ <- char('{')
    _ <- char('}')
  } yield JsonObject(Map.empty)

  def jsonObj: Parser[JsonObject] = (for {
    _ <- char('{')
    x <- jsonObjItem
    xs <- many(jsonObjNextItem)
    _ <- char('}')
  } yield JsonObject((x :: xs).toMap)) +++ jsonEmptyObj

  def jsonNextValue = for {
    _ <- char(',')
    i <- jsonValue
  } yield i

  def jsonEmptyArr: Parser[JsonArray] = for {
    _ <- char('[')
    _ <- char(']')
  } yield JsonArray(Vector.empty)

  def jsonArr: Parser[JsonArray] = (for {
    _ <- char('[')
    x <- jsonValue
    xs <- many(jsonNextValue)
    _ <- char(']')
  } yield JsonArray((x :: xs).toVector)) +++ jsonEmptyArr

  println(jsonNum.parse("123"))

  println(jsonStr.parse("\"hello\""))

  println(jsonObjItem.parse("\"key\":\"value\""))
  println(jsonObjItem.parse("\"key\":123"))

  println(jsonObjNextItem.parse(",\"key\":\"value\""))
  println(jsonObjNextItem.parse(",\"key\":123"))

  println(jsonObj.parse("{\"key\":\"value\",\"k2\":[1,2,3]}"))
  println(jsonObj.parse("{}"))

}
