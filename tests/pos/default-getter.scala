class X
class Y extends X

class A {
  def foo(param: X = new Y): X = param
}

class B extends A {
  override def foo(param: X = new X): X = param
}
