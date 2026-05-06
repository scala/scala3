//> using options -feature -preview

import Conversion.into

class Text(val str: String)

object Test:

  given Conversion[String, Text] = Text(_)

  def f(x: Text, y: => Text, zs: Text*) =
    println(s"${x.str} ${y.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // warn // warn
  f("abc", "def", "xyz", "uvw")  // warn // warn // warn // warn
  f("abc", "def", "xyz", Text("uvw"))  // warn // warn // warn

  def g(x: into[Text]) =
    println(x.str)

  def g2(x: into[Text]) =
    println(x.str)

  def g3(x: Text) =
    println(x.str)

  g("abc")  // OK
  val gg = g
  gg("abc") // ok

  val c1 = if ??? then g else g2
  c1("abc") // ok, lub type = into[Text] => Unit

  val c2 = if ??? then g else g3
  c2("abc") // warn, lub type is Text => Unit

  val c3 = if ??? then g3 else g
  c3("abc") // warn, lub type is Text => Unit

  def h1[X](x: X)(y: X): Unit = ()

  def h(x: into[Text]) =
    val y = h1(x)
    y("abc")  // ok