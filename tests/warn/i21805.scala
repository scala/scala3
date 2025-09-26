//> using options -Wunused:imports

def i23967: Boolean = {
  //import scala.compiletime.testing.typeCheckErrors
  import scala.compiletime.testing.* // nowarn
  typeChecks("2 + 2")
}

package p:
  val code = """"hello, world""""
package c:
  class C(i: Int)

package q:
  import c.* // warn should be nowarn
  import p.* // warn should be nowarn
  import scala.compiletime.testing.*
  def test() = typeCheckErrors("""println(C("hello, world"))""")
  def ok() = typeChecks("println(code)")
  inline def f(inline i: Int) = 42 + i

package i23967b:
  package ok:
    import scala.compiletime.testing.* // nowarn
    def test() = typeChecks("42 + 27")
  package nok:
    import scala.compiletime.testing.typeChecks // nowarn
    def test() = typeChecks("42 + 27")

@main def Test = println:
  q.f(27)
