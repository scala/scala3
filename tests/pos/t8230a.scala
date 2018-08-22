trait Arr[T]
object Arr {
  def apply[T](xs: T): Arr[T] = ???
  def apply(x: Long) : Arr[Long] = ???
}

object I {
  implicit def arrToTrav[T] (a: Arr[T])   : Traversable[T] = ???
  implicit def longArrToTrav(a: Arr[Long]): Traversable[Long] = ???
}

object Test {
  def foo(t: Traversable[Any]) = {}

  object Okay {
    Arr("1")

   import I.{ arrToTrav, longArrToTrav }
   val x = foo(Arr("2"))
 }

  object Fail {
    import I.arrToTrav
    foo(Arr("3")) // found String, expected Long
  }
}
