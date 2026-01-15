def test(c: Object^) =
  class E:
    def f = println(c)
  class A extends E, caps.Mutable // error

class B(x: Int => Int)
class C extends B(??? : Int => Int), caps.Mutable // error

class D extends caps.Mutable:
  var x: Int = 0

class F extends D, caps.Mutable // ok

class Ref extends caps.Mutable

class G:
  val r: Ref^ = Ref()
class H extends G, caps.Mutable // error


