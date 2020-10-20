import scala.quoted._

def summonTypedType[T : Type](using QuoteContext): String = '{ ??? : T } match {
  case '{ $_ : Boolean } => "Boolean"
  case '{ $_ : Byte } => "Byte"
  case _ => "Other"
}
