import scala.quoted.*

given staging.Compiler =
  object Dummy
  staging.Compiler.make(Dummy.getClass.getClassLoader)

class A(i: Int)

def f(i: Expr[Int])(using Quotes): Expr[A] = { '{ new A($i) } }

@main def Test = {
  val g: Int => A = staging.run { '{ (i: Int) => ${ f('{i}) } } }
  println(g(3))
}
