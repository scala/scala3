package test

class C[T] {

    def foo(x: D) = { System.out.println("D foo"); }
    def foo(x: T) = { System.out.println("T foo"); }
}

object C {
  def main(args: Array[String]) =
        new C[D]().foo(new D())
}

class C1[T] {
    def foo(x: D) = { System.out.println("D foo"); }
}
class C2[T] {
    def foo(x: D) = { System.out.println("D foo"); }
}

class D {}

class X {
  def foo(x: D): D
  def foo(x: D): D // error: already defined
}
