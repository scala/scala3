def foo[A, B](arr: Array[A], pf: PartialFunction[A, B]): Seq[B] = arr.toSeq.collect(pf)
def foo[A, B](list: List[A], pf: PartialFunction[A, B]): Seq[B] = list.collect(pf) // no errors if this is commented out

val arr = Array(1, 2, 3)
val resOkay = foo(arr = arr, { case n if n % 2 != 0 => n.toString }) // compiles
val resNope = foo(arr = arr, pf = { case n if n % 2 != 0 => n.toString }) // Error 1
val resNope2 = foo[Int, String](arr = arr, pf = { case n if n % 2 != 0 => n.toString }) // Error 2