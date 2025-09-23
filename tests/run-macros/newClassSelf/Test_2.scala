@main def Test: Unit = {
  val a: Bar = makeClass("A")
  a.bar()
  callFoo(a)
  println(a.getClass)
  val b: Bar = makeClass("B")
  b.bar()
  callFoo(b)
  println(b.getClass)
}

def callFoo(x: Any): Unit =
  x.getClass.getMethod("foo").invoke(x)
