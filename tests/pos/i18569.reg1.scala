// Minimisation of the CI failure
// in scala-parallel-collections
// to do with how EtaExpansion is used
// by typeOfNew when typing a New tree

trait Foo[+A]:
  class Bar[B >: A]

class Test:
  def t1[X](foo: Foo[X]): Unit =
    val bar = new foo.Bar()
