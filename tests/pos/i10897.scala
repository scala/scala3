import Tuple.Union

object Foo

val x = summon[Union[(Foo.type, 1)] =:= (Foo.type | 1)] // doesn't compile
val y = summon[Union[(Foo.type, 1, String)] =:= (Foo.type | 1 | String)] // compiles
