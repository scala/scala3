type second[X <: Tuple2[Any, Any]] = Tuple.Head[Tuple.Tail[X]]
type middle[X <: Tuple3[Any, Any, Any]] = Tuple.Head[Tuple.Tail[X]]

val a: Tuple.Head[Tuple.Tail[Tuple2[Int, String]]] = ???
val b: Tuple.Head[Tuple.Tail[Tuple3[Int, String, Boolean]]] = ???
