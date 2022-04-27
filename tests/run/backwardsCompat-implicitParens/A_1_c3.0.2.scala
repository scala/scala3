class Bar(using x: Int)(y: String):
  override def toString = "Bar"
object Bar:
  given Int = 1
  inline def foo =
    println(new Bar()(""))
    println(Bar()(""))

class Bat(using x: Int):
  override def toString = "Bat"
object Bat:
  given Int = 1
  inline def foo =
    println(new Bat())
    println(Bat())

class Bax(using x: Int)():
  override def toString = "Bax"
object Bax:
  given Int = 1
  inline def foo =
    println(new Bax())
    println(Bax())

class Baz(using x: Int)(using y: String):
  override def toString = "Baz"
object Baz:
  given Int = 1
  given String = "x"
  inline def foo =
    println(new Baz())
    println(Baz())
