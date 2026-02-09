import language.experimental.erasedDefinitions

object Test extends App {

  // Types
  type F1 = [T] => (erased x: T) => Int
  type F2 = [T, U] => (t: T, erased u: U) => T

  // Terms
  val t1 = [T] => (erased t: T) => 3
  assert(t1(List(1, 2, 3)) == 3)
  val t1a: F1 = t1
  val t1b: F1 = [T] => (erased t) => 3
  assert(t1b(List(1, 2, 3)) == 3)

  val t2 = [T, U] => (t: T, erased u: U) => t
  assert(t2(1, "abc") == 1)
  val t2a: F2 = t2
  val t2b: F2 = [T, U] => (t, erased u) => t
  assert(t2b(1, "abc") == 1)

}
