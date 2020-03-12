class Contra[-T >: Null]

object Test:
  def foo =   // error
    class A
    new Contra[A]

  val x = foo
