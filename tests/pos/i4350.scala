import scala.quoted.{_, given}

class Foo[T: Type](given QuoteContext) {
  '{null.asInstanceOf[T]}
}
