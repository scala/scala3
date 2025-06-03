//> using options -Xfatal-warnings

import language.experimental.into

class Text(val str: String)

given Conversion[String, Text] = Text(_)

trait A[X]:
  def f(x: X): Unit = ()

trait B[X] extends A[X]:
  override def f(x: X) = super.f(x)

trait C[X] extends A[X]:
  override def f(x: into X) = super.f(x)    // error

class D[X] extends B[X], C[X] // error

trait E[X] extends C[X]:
  override def f(x: X) = super.f(x) // error

def f = new D[Text].f("abc")
