class Foo {
  1 match {
    case 1 => 2
    case _ => Effect.canThrowNPE // error
  }
}
