class Tup():
  def app(n: Int): String = "a"

class NT(t: Tup):
  def toTup = t
object NT:
  extension (x: NT)
    def app(n: Int): Boolean = true
  given c1: Conversion[NT, Tup] = _.toTup
  implicit def c2(t: NT): Tup = c1(t)

def test =
  val nt = new NT(Tup())
  val x = nt.app(3)
  val _: Boolean = x

