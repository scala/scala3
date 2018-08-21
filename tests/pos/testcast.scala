package test

class A;

class B extends A {
  def foo: Int = 1
}

object B {
  def view(x: B): B1 = ???
}

class B1 {
  def bar: Int = 1
}

object C {
  implicit def view(x: A): B1 = ???
}

object Test {
  import C.view

  val b: B = ???

  println(b.bar)
}
