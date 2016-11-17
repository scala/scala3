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
  def useDep(d: Dep) {  // error: procedure syntax
    import d._
    a m (b)
  }
  {   // error: Null does not take parameters (follow on)
    import dep._
    a m (b)
  }
  dep.a m (dep b) // error (follow on)
}
