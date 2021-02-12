import scala.quoted.*

class Foo[T: Type](using Quotes) {
  '{null.asInstanceOf[T]}
}
