package test

import caps.SharedCapability

class File

object Console extends SharedCapability:
  def println(s: String) = Predef.println(s)

class A uses Console {
  val f: File^ = File()
  val g: File^ = File()

  class B uses A.this.f, Console:
    def show =
      Console.println(f.toString)
      Console.println(g.toString) // error

  class C uses Console:
    def show =
      Console.println(f.toString) // error
}

def test =
  val a = A()
  val b = a.B()
  val _: a.B^{a.f} = b // error

class A0 uses Console {

  class B:
    def show =
      Console.println("") // error

  class C uses Console:
    def show =
      Console.println("") // ok
}

class A1 {
  class B uses Console:
    def show =
      Console.println("") // error
}