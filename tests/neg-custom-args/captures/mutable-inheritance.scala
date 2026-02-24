def test(c: Object^) =
  class E:
    def f = println(c)
  class A extends E, caps.Stateful // error

class B(x: Int => Int)
class C extends B(??? : Int => Int), caps.Stateful // error

class D extends caps.Stateful:
  var x: Int = 0

class F extends D, caps.Stateful // ok

class Ref extends caps.Stateful

class G:
  val r: Ref^ = Ref()
class H extends G, caps.Stateful // error


