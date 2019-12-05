
inline def foo(x: Int): Int =
  inline x match
    case 1 => 9

object Foo {
  val a = foo(1)
  val b = foo(2) // error
}