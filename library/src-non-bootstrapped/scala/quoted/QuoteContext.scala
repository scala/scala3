package scala.quoted

trait QuoteContext { self =>

  val reflect: scala.tasty.Reflection

  type Nested = QuoteContext {
    val reflect: self.reflect.type
  }

}
