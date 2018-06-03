object TypeClassDemo extends App {
  println("Hello, world!")

  val jsonConvertable = new JsonConvertable[Expression] {
    override def convertToJson(value: Expression): JsonValue = value match {
      case Number(value: Int) => JNumber(value)
      case Plus(lhs: Expression, rhs: Expression) => JObject(
        Map(
          "op" -> JString("+"),
          "lhs" -> convertToJson(lhs),
          "rhs" -> convertToJson(rhs)
        )
      )
      case Minus(lhs: Expression, rhs: Expression) => JObject(
        Map(
          "op" -> JString("-"),
          "lhs" -> convertToJson(lhs),
          "rhs" -> convertToJson(rhs)
        )
      )
    }
  }

  val expression: Expression = Plus(Number(1), Number(2))
  val expressionJsonStr = JsonWriter.write(expression)(jsonConvertable)
  println(expressionJsonStr)
}


sealed trait Expression
case class Number(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression





sealed trait JsonValue
case class JObject(entitis: Map[String, JsonValue]) extends JsonValue
case class JArray(entitis: List[JObject]) extends JsonValue
case class JNumber(value: Int) extends JsonValue
case class JString(value: String) extends JsonValue
case class JBool(value: Boolean) extends JsonValue
object JNull extends JsonValue

trait JsonConvertable[T] {
  def convertToJson(value: T): JsonValue
}

object JsonWriter {
  def write(jValue: JsonValue): String = jValue match {
    case JObject(entitis: Map[String, JsonValue]) => {
      val serializeEntities = for((key, value) <- entitis) yield key + ":" + write(value)
      "{" + serializeEntities.mkString(", ") + "}"
    }
    case JArray(entitis: List[JObject]) => {
      "[" + entitis.mkString(",") + "]"
    }
    case JNumber(value: Int) => value.toString
    case JString(value: String) => value
    case JBool(value: Boolean) => {
      if(true) "true"
      else "false"
    }
    case JNull => "null"
  }

  def write[A](a: A)(cov: JsonConvertable[A]): String = {
    write(cov.convertToJson(a))
  }
}


