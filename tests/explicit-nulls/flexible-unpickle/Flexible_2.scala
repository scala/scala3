@main
def Flexible_2() =
  val s2: String | Null = "foo"
  val unsafe = new Unsafe_1()
  val s: String = unsafe.foo(s2)
  unsafe.foo("")
  unsafe.foo(null)