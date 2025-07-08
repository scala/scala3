
case class Foo()

type T = Tuple.Elem[(Foo, Any), 0]
type U = Tuple.Elem[(Any, T), 1]

val _ = new T() // ok
val _ = new U() // ok
