type A
type B <: A

type PartialId[X] = X match {
  case B => X
}

trait T1[T] {
  extension (t1: T) def idnt1: Any
}

given [T <: A] => PartialId[T] => T1[T] = new T1[T] {
  extension (t1: T) def idnt1: Any = ???
}

given PartialId[B] = ???

val x: B = ???
val z = x.idnt1  // used to be an error, now ok

