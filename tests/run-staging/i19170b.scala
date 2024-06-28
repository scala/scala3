import scala.quoted.*

given staging.Compiler =
  staging.Compiler.make(getClass.getClassLoader) // warn: Suspicious top-level unqualified call to getClass

class A(i: Int)

def f(i: Expr[Int])(using Quotes): Expr[A] = { '{ new A($i) } }

@main def Test = {
  // The heuristic to give the extra information does not work on JDK 8
  if System.getProperty("java.specification.version") != "1.8" then
    try
      val g: Int => A = staging.run { '{ (i: Int) => ${ f('{i}) } } }
      println(g(3))
    catch case ex: Exception =>
      assert(ex.getMessage().startsWith("`scala.quoted.staging.run` failed to load a class."), ex.getMessage())
}
