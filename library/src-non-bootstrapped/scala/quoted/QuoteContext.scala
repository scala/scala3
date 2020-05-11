package scala.quoted

trait QuoteContext { self =>

  val tasty: scala.tasty.Reflection

  type Nested = QuoteContext {
    val tasty: self.tasty.type
  }

}
