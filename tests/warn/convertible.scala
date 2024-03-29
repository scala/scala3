//> using options  -feature

import language.experimental.into

class Text(val str: String)

object Test:

  given Conversion[String, Text] = Text(_)

  def f(x: Text, y: => Text, zs: Text*) =
    println(s"${x.str} ${y.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // warn // warn
  f("abc", "def", "xyz", "uvw")  // warn // warn // warn // warn
  f("abc", "def", "xyz", Text("uvw"))  // warn // warn // warn

  def g(x: into Text) =
    println(x.str)


  g("abc")  // OK
  val gg = g
  gg("abc") // straight eta expansion is also OK

  def h1[X](x: X)(y: X): Unit = ()

  def h(x: into Text) =
    val y = h1(x)
    y("abc")  // warn, inference through type variable does not propagate