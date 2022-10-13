import language.experimental.namedTypeArguments

class Map[Key, Value](elems: (Key, Value)*)

val m1: Map[Int, _] = ???   // error
val m2: Map[_, String] = ??? // error
val m4: Map[Int, _, String] = ??? // error

