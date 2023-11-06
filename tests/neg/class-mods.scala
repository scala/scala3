open final class Foo1       // error
sealed open class Foo2      // error

open type T1     // error
sealed type T2   // error
abstract type T3 // error
abstract open type T4 // error

object foo {
  abstract val x: Int = 1  // error
}