class Foo {
  try 1
  catch { case ex: Exception => Effect.canThrowNPE } // error
}
