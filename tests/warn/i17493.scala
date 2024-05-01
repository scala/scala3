//> using options -explain
class A(val s: String) extends AnyVal {
  // def f = eq("hello, world") // no warning for now because `eq` is inlined
  def g = synchronized { println("hello, world") } // warn
}
