
case class A(x: String)
case class B(x: String)
given A("default")
given B("default")
val a = A("explicit")
val b = B("explicit")

def f(using a: A, b: B): Unit =
  println(a)
  println(b)

@main def Test =
  f(using a)
  f(using a = a)
  f(using b = b)
  f(using b = b, a = a)

