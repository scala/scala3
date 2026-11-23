// usage_2.scala
import scala.language.implicitConversions
class Bug {
  def searchJson = Json.obj("foo" -> "bar")
}