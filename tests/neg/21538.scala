trait Bar[T]
given [T]: Bar[T] with {}
inline def foo[V](inline value: V)(using Bar[value.type]) : Unit = {} // error