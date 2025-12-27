package test

import caps.SharedCapability

class File

object Console extends SharedCapability:
  def println(s: String) = Predef.println(s)

class A {
  val f: File^ = File()
  val g: File^ = File()

  class B uses A.this.f:
    def show =
      Console.println(f.toString)
      Console.println(g.toString) // error

  class C: // error
    def show =
      Console.println(f.toString)
}

def test =
  val a = A()
  val b = a.B()
  val _: a.B^{a.f} = b
