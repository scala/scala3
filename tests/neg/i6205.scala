class Contra[-T >: Null]

object Test with
  def foo =   // error
    class A
    new Contra[A]

  val x = foo
