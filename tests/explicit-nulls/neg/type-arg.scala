// Test that reference types being non-nullable
// is checked when lower bound of a type argument
// is Null.

object Test {
  type Untyped = Null
  class TreeInstances[T >: Untyped]
  class Type

  object untpd extends TreeInstances[Null]
  // There are two errors reported for the line below (don't know why).
  object tpd extends TreeInstances[Type] // error // error
}
