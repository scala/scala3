import language.experimental.packageObjectValues

package a:
  package object b:
    class Foo:
      println("Foo was created")

    def foo() = Foo()
  end b

  def test =
    val bb = b
    bb.foo()
    new bb.Foo()
end a

@main def Test =
  a.test
  val ab: a.b.type = a.b
  ab.foo()
  new ab.Foo()

