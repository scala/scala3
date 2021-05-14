transparent trait A
transparent trait B
trait C

object Test:
  val x = identity(new A with B) // infers A with B (because there's no non-transparent trait in the intersection)
  val x2: A with B = x // OK

  val y = identity(new A with B with C) // infers C
  val y2: C = y // OK
  val y3: A with B = y // error
