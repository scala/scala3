// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

given e: E(null) with {}

object Test extends App {

  given f(using e: E): E(e) with {}

  assert(summon[E].toString == "E(E(null))")

}