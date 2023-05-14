import language.postfixOps
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
    import d.*
    a m (b)
  }
  {
    import dep.*
    a m (b) // error: not found: a
  }
  dep.a m (dep b) // error (follow on)
}
