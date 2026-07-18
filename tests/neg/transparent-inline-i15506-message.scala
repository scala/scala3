transparent inline def foo: Int | Boolean = 2

transparent inline def bar: Option[Int | Boolean] = Some(foo)

val x: Some[Int] = bar // error
