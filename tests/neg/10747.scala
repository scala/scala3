type Foo[A] = A match {
  case Int => String
}

type B = Foo[Boolean]
val _: B = "hello" // error
