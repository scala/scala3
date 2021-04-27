// Checks that divergence checking works before going into
// recursions.
case class E(x: E | Null)

given e: E(null)

object Test extends App {

  given f(using e: E): E(e)

  assert(summon[E].toString == "E(E(null))")

}