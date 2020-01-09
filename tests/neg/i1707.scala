object DepBug {
  class A {
    class B
    def mkB = new B
    def m(b: B) = b
  }
  trait Dep {
    val a: A
    val b: a.B
  }
  val dep = new {
    val a = new A
    val b = a mkB
  }
  def useDep(d: Dep) {  // error: '=' expected, but '{' found
    import d._
    a m (b)
  }
  {
    import dep._
    a m (b) // error: not found: a
  }
  dep.a m (dep b) // error: value a is not a member of Object (follow on)
}
