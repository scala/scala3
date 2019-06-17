// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

delegate e for E(null)

object Test extends App {

  delegate f for E(e) given (e: E)

  assert(the[E].toString == "E(E(null))")

}