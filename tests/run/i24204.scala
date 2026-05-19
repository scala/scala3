class NullArgumentException(msg: String) extends IllegalArgumentException(msg)

trait Suite
class Suites(suitesToNest: Suite*) {
  if suitesToNest == null then
    throw NullArgumentException("The passed suites splice is null")

  for
    s <- suitesToNest // triggers NullPointerException
    if s == null
  do
    throw new NullArgumentException("One of the passed suite was null")
}

@main def Test = {
  val aNull: Array[Suite] = null
  try {
    new Suites(aNull*)
    throw IllegalStateException("Should not be reached")
  } catch {
    case ex: NullArgumentException => println(s"ok, expected: $ex")
  }
}