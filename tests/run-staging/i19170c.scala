import scala.quoted.*

given staging.Compiler =
  staging.Compiler.make(getClass.getClassLoader.getParent) // different classloader that 19170b.scala
class A(i: Int)

def f(i: Expr[Int])(using Quotes): Expr[A] = { '{ new A($i) } }

@main def Test = {
  try
    val g: Int => A = staging.run { '{ (i: Int) => ${ f('{i}) } } }
    println(g(3))
  catch case ex: Exception =>
    assert(ex.getMessage().startsWith("An unhandled exception was thrown in the staging compiler."), ex.getMessage())
    println("exception thrown, no additional printlns")
}
