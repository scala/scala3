import caps.SharedCapability

class File

object Console extends SharedCapability:
  def println(s: String) = Predef.println(s)

class A {
  val f: File^ = File()

  class B uses A.this.f, Console:
    def show = Console.println(f.toString)
}

def test =
  val a = A()
  val b = a.B()
  val _: a.B^{a.f} = b
