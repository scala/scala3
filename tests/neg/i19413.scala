
import hello.bla.* // error

//transparent inline def hello: kla.type = kla // ok
def hello: kla.type = kla

object kla:
  def ra = ???
  object bla { val ra1 = 31 }

//import kla.bla.ra1

@main def test = println:
  ra1
