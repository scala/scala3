class First[A]
class Second[A]

class Foo {
  def foo[A: First] = {
    def bar[B: Second] = {
      val fst: First[A] = implicitly[First[A]]
      val snd: Second[B] = implicitly[Second[B]]
    }
  }
}
