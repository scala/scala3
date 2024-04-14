
case class Foo()

type T = Tuple.Elem[(Foo, Any), 0]

val x = new T() // ok

