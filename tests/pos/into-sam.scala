
//> using options -feature -Werror -preview

import Conversion.into

class Text(val str: String)

given Conversion[String, Text] = Text(_)
object Test:
  def f(x: Int)(y: into[Text]): Unit = ()
  val _: Text => Unit = f(3)

  trait ConvArg:
    def apply(x: into[Text]): Unit

  val x: ConvArg = f(3)(_)

  x("abc")
