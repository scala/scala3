class File

class A {
  val f: File^ = File()

  class B uses A.this.f:
    def show = Console.println(f.toString)
}

def test =
  val a = A()
  val b = a.B()
  val _: a.B^{a.f} = b
