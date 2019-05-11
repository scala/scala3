// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

implicit e for E(null)

object Test extends App {

  implicit f for E(e) given (e: E)

  assert(the[E].toString == "E(E(null))")

}