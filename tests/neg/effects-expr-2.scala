class Foo {

  val b = true

  if (b) Effect.canThrowNPE // error
  else "abc"

  if (b) "abc"
  else Effect.canThrowNPE // error

  1 match {
    case 1 => 2
    case _ => Effect.canThrowNPE // error
  }

  try 1
  catch { case ex: Exception => Effect.canThrowNPE } // error
}
