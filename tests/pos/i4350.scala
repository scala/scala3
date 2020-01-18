import scala.quoted._

class Foo[T: Type] with QuoteContext {
  '{null.asInstanceOf[T]}
}
