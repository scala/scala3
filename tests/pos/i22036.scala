type Foo[T] = T
val x: NamedTuple.From[Tuple.Map[(Int, Int), Foo]] = ???
val res = x._1

type Z = NamedTuple.From[(Foo[Int], Foo[Int])]
val x2: Z  = ???
val res2 = x2._1

val x3: Foo[NamedTuple.From[Tuple.Map[(Int, Int), Foo]]]  = ???
val res3 = x3._1

def foo[T <: (Int, String)](tup: T): Int =
  tup._1

def union[T](tup: (Int, String)
|(Int, String)
): Int =
  tup._1

def intersect[T](tup: (Int, String)
& T
): Int =
  tup._1