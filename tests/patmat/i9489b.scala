import scala.quoted.*

def summonTypedType[T : Type](using Quotes): String = '{ ??? : T } match {
  case '{ $_ : Boolean } => "Boolean"
  case '{ $_ : Byte } => "Byte"
  case _ => "Other"
}
