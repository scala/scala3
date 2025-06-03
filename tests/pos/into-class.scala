import language.experimental.into

class Text(str: String)

case class C(x: into Text)

case class D(x: Text)

given Conversion[String, Text] = Text(_)

def Test =
  val c = C("a")
  val d = new C("b")
  val e = c.copy()
  val f = c.copy(x = "d")
