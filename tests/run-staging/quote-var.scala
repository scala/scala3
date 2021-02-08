import scala.quoted.*
import scala.quoted.staging.*

object Test {

  sealed trait Var {
    def get(using Quotes): Expr[String]
    def update(x: Expr[String])(using Quotes): Expr[Unit]
  }

  object Var {
    def apply(init: Expr[String])(body: Var => Expr[String])(using Quotes): Expr[String] = '{
      var x = $init
      ${
        body(
          new Var {
            def get(using Quotes): Expr[String] = 'x
            def update(e: Expr[String])(using Quotes): Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1()(using Quotes): Expr[String] = Var('{"abc"}) { x =>
    '{
      ${ x.update('{"xyz"}) }
      ${ x.get }
    }
  }

  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    val res = run {
      test1()
    }
    println(res)
  }
}



