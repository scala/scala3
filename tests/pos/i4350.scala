import scala.quoted._

class Foo[T: TypeTag](given QuoteContext) {
  '{null.asInstanceOf[T]}
}
