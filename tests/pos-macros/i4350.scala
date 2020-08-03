import scala.quoted._

class Foo[T: Staged](using QuoteContext) {
  '{null.asInstanceOf[T]}
}
