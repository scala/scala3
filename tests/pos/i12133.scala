class B

class A {
  def foo(x: B) = ???
  def foo(str: String) = ???
}

//implicit class C(x: A) {
//  def foo(s: Int*) = s.size
//}
extension (x: A) def foo(s: Int*) = s.size

val a = new A

def test: Unit = a.foo(1, 2)