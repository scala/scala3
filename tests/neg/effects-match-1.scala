class Foo {
  Effect.canThrowNPE match { // error
    case _: Pure => ()
  }
}
