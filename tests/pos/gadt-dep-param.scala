enum Tup[A, B]:
  case Data[A, B]() extends Tup[A, B]

def foo1[A, B](e: Tup[A, B]) = e.match {
  case _: Tup.Data[a, b] =>
    def bar[C >: a <: b, D]() =
      val t1: b = ??? : a
}

def foo2[A, B, C >: A <: B]() =
  val t1: B = ??? : A

