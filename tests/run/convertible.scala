//> using options -feature -Xfatal-warnings

import language.experimental.into

class Text(val str: String)

given Conversion[String, Text] = Text(_)

@main def Test =

  def f(xxx: into Text, yyy: => into Text, zs: (into Text)*) =
    println(s"${xxx.str} ${yyy.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // ok
  f("abc", "def", "xyz", "uvw")  // ok
  f("abc", "def", "xyz", Text("uvw"))  // ok

  def g(x: () => into Text) =
    println(x().str)

  g(() => "hi")

trait C[X]:
  def f(x: into X) = x

class D[X] extends C[X]

def f = new D[Text].f("abc")


