// This one is unavoidable. Dotty does not allow several overloaded
// parameterless methods, so it picks the one in the subclass.

import scala.language.higherKinds
// Minimal reproduction for:
// scala.collection.mutable.ArrayStack.empty[Int]

abstract class Super[C[_]] {
  def empty[T]: C[T] = ???
}

class Child[T]

object Child extends Super[Child] {
  def empty: Child[Nothing] = new Child()

  Child.empty[Int]
}
