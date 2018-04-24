class Foo {
  1 match {
    case 1 => 2
    case _ => Effect.canThrowNPE // error
  }

  try Effect.canThrowNPE // error
  finally ()

  try ""
  catch {
    case ex: Exception => Effect.canThrowNPE // error
  }
}
