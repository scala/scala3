// scalac: -Xfatal-warnings -feature

import language.experimental.into

class Text(val str: String)

object Test:

  given Conversion[String, Text] = Text(_)

  def f(x: Text, y: => Text, zs: Text*) =
    println(s"${x.str} ${y.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // error // error
  f("abc", "def", "xyz", "uvw")  // error // error // error // error
  f("abc", "def", "xyz", Text("uvw"))  // error // error // error

  def g(x: into Text) =
    println(x.str)


  g("abc")  // OK
  val gg = g
  gg("abc") // straight eta expansion is also OK

  def h1[X](x: X)(y: X): Unit = ()

  def h(x: into Text) =
    val y = h1(x)
    y("abc")  // error, inference through type variable does not propagate

