import scala.language.experimental.namedTuples

class Test[S <: String & Singleton](name: S):

  type NT = NamedTuple.NamedTuple[(S, "foo"), (Int, Long)]
  def nt: NT = ???

  type Name = S
  
  type NT2 = NamedTuple.NamedTuple[(Name, "foo"), (Int, Long)]
  def nt2: NT2 = ???

def test =
  val foo = new Test("bar")
  
  foo.nt.bar
  foo.nt2.bar
