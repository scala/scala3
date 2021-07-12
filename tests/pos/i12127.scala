val x = Option((1, 2, 3)).map(_ + _ + _)
def foo[T <: Tuple : Tuple.IsMappedBy[Option]](t: T)(f: Tuple.InverseMap[T, Option] => Int) = null
val y = foo(Option(1), Option(2), Option(3))(_ + _ + _)

//val x: (Tuple3[Int, Int, Int] => Int) = _ + _ + _
