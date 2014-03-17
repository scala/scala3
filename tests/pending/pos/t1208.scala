object Test {
  object Foo
  val f: Option[Foo.type] = Some(Foo)
}

// unsupported with current typing rules.
// on the other hand, we need a way to refer to a module class.
