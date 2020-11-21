import scala.quoted._

class Foo[T: Type](using Quotes) {
  '{null.asInstanceOf[T]}
}
