// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

given e as E(null)

object Test extends App {

  given f as E(e) given (e: E)

  assert(summon[E].toString == "E(E(null))")

}