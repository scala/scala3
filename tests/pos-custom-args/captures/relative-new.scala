class File

class A:
  val f: File^ = File()

  class B:
    def show = println(f)

def test =
  val a = A()
  val b = a.B()
  val _: a.B^{a.f} = b
