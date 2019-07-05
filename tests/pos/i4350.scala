import scala.quoted._

class Foo[T: Type] given QuoteContext {
  '{null.asInstanceOf[T]}
}
