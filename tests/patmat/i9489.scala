import scala.quoted.*

def summonTypedType[T : Type](using Quotes): String = Type.of[T] match {
  case '[Boolean] => "Boolean"
  case '[Byte] => "Byte"
  case _ => "Other"
}
