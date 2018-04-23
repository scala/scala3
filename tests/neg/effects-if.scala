class Foo {

  val b = true

  if (b) Effect.canThrowNPE
  else "abc" // error

  if (b) "abc"
  else Effect.canThrowNPE // error

}
