def species(using quoted.Quotes) = '{
  case object Bar // error
  case class FooT() // error
  ${
    case object Baz // error
    ???
  }
  FooT()
}
