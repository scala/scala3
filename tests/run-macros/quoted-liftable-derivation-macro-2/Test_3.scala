object Test extends App {
  import Opt._
  assert(Lib.optTwo == Sm(2))
  assert(Lib.smTwo == Sm(2))
  assert(Lib.none == Nn)
}
