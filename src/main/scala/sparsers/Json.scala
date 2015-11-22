package sparsers

sealed trait JsonValue

case class JsonString(str: String) extends JsonValue

case class JsonNumber(n: Double) extends JsonValue

case class JsonObject(obj: Map[JsonString, JsonValue]) extends JsonValue

case class JsonArray(arr: Vector[JsonValue]) extends JsonValue

case class JsonBoolean(b: Boolean) extends JsonValue

case object JsonNull extends JsonValue
