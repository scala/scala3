import scala.quoted._

class Foo[T: Type](using QuoteContext) {
  '{null.asInstanceOf[T]}
}
