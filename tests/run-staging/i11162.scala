import scala.quoted.*

object Test {

  given staging.Compiler = staging.Compiler.make(this.getClass.getClassLoader)

  def main(args: Array[String]): Unit =
    staging.run {
      '{ foo() }
    }

  inline def foo(): Unit = ${ fooExpr() }

  private def fooExpr()(using Quotes): Expr[Unit] = '{ println("foo") }
}
