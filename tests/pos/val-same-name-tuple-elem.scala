// https://github.com/scala/scala3/issues/23893
val foo: (f: String) = "" *: EmptyTuple
val f = foo.f // previously: error: recursive value foo needs type
