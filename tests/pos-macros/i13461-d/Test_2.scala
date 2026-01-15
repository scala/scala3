trait Leak[T]:
  type Out
given [T]: Leak[T] with
  type Out = T
extension [T](t: T)(using l: Leak[T]) def leak: l.Out = ???

val x = MyOpaque().leak
val shouldWork = summon[x.type <:< MyOpaque]
