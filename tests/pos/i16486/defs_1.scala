// defs_1.scala
import java.time.*

type Temporal =
  java.sql.Date |
  LocalDateTime | LocalDate | LocalTime |
  Instant

given Conversion[String | Temporal, JsValue] =  ???

sealed trait JsValue
case class JsObject(value: Map[String, JsValue])

object Json{
  def obj(fields: Tuple2[String,  JsValue | Option[JsValue]]* ): JsObject = ???
}

