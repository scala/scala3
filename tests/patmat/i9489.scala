import scala.quoted._

def summonTypedType[T : Type](using QuoteContext): String = Type.of[T] match {
  case '[Boolean] => "Boolean"
  case '[Byte] => "Byte"
  case _ => "Other"
}
