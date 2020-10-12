package scala.quoted

trait QuoteContext { self =>

  val reflect: scala.tasty.Reflection
  def tasty: reflect.type = reflect

  type Nested = QuoteContext {
    val tasty: self.tasty.type
  }

}
