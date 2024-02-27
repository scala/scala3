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

  def g2(x: into Text) =
    println(x.str)

  def g3(x: Text) =
    println(x.str)

  g("abc")  // OK
  val gg = g
  gg("abc") // warn, eta expansion does not preserve into

  val c1 = if ??? then g else g2
  c1("abc") // warn, eta expansion does not preserve into

  val c2 = if ??? then g else g3
  c2("abc") // warn, eta expansion does not preserve into

  val c3 = if ??? then g3 else g
  c3("abc") // warn, eta expansion does not preserve into

  def h1[X](x: X)(y: X): Unit = ()

  def h(x: into Text) =
    val y = h1(x)
    y("abc")  // warn, eta expansion does not preserve into