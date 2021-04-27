
case class A(x: String)
case class B(x: String)
given a1: A("default")
given b1: B("default")
val a2 = A("explicit")
val b2 = B("explicit")

def f(using a: A, b: B): Unit =
  println(a)
  println(b)

@main def Test =
  f(using a2)
  f(using a = a2)
  f(using b = b2)
  f(using b = b2, a = a2)

