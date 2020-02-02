import scala.quoted._
import scala.quoted.staging._

object Test {

  sealed trait Var {
    def get(using QuoteContext): Expr[String]
    def update(x: Expr[String])(using QuoteContext): Expr[Unit]
  }

  object Var {
    def apply(init: Expr[String])(body: Var => Expr[String])(using QuoteContext): Expr[String] = '{
      var x = $init
      ${
        body(
          new Var {
            def get(using QuoteContext): Expr[String] = 'x
            def update(e: Expr[String])(using QuoteContext): Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1()(using QuoteContext): Expr[String] = Var('{"abc"}) { x =>
    '{
      ${ x.update('{"xyz"}) }
      ${ x.get }
    }
  }

  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    val res = run {
      test1()
    }
    println(res)
  }
}



