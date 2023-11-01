// lampepfl/dotty#14576, Writer is in a separate file and not declared `open`
//   under -source:future this requires -language:adhocExtensions
class ExtWriter extends Writer

class Text(val str: String)

object Test:
  // lampepfl/dotty#14500, requires implicitConversions feature
  given Conversion[String, Text] = Text(_)
  def f(x: Text) = println(x.str)
  f("abc")

  // under -source:future, `_` is deprecated for wildcard arguments of types: use `?` instead
  val xs: List[_] = Nil
