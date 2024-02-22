//> using options -Xfatal-warnings -Yno-experimental

import language.experimental.into

class Text(val str: String)

given Conversion[String, Text] = Text(_)

@main def Test =

  def f(x: into Text, y: => into Text, zs: into Text*) =
    println(s"${x.str} ${y.str} ${zs.map(_.str).mkString(" ")}")

  f("abc", "def")  // ok
  f("abc", "def", "xyz", "uvw")  // ok
  f("abc", "def", "xyz", Text("uvw"))  // ok

  def g(x: into () => Text) =
    println(x().str)

  g(() => "hi")

trait A[X]:
  def f(x: X): Unit = ()

trait B[X] extends A[X]:
  override def f(x: X) = super.f(x)

trait C[X] extends A[X]:
  override def f(x: into X) = super.f(x)

class D[X] extends B[X], C[X]

def f = new D[Text].f("abc")
