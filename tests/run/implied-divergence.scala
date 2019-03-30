// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

implied e for E(null)

object Test extends App {

  implied f given (e: E) for E(e)

  assert(the[E].toString == "E(E(null))")

}