class Foo {
  Effect.canThrowNPE match {
    case _: Pure => () // error
  }
}
