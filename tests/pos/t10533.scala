object Foo {
  val Bar(_) as b = Bar(1)(2)(3)
}

case class Bar(a: Int)(b: Int)(c: Int)
