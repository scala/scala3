trait SubBase {
  type A0
  type B0
  type C >: A0 <: B0
}
trait Tag { type A }

type Sub[A, B] = SubBase { type A0 = A; type B0 = B }

def foo(x: Tag, y: Tag, e: Sub[x.A, y.A]) = e.match {
  case _: Object =>
    val t1: y.A = ??? : x.A
}

def bar(x: Tag, e: Sub[Int, x.A]): x.A = e match {
  case _: Object => 0
}

def baz(x: Tag, e: Sub[x.A, Int]): Int = e match {
  case _: Object => ??? : x.A
}
