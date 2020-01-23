import scala.quoted._
import scala.quoted.staging._

object Test {

  sealed trait Var {
    def get with QuoteContext : Expr[String]
    def update(x: Expr[String]) with QuoteContext : Expr[Unit]
  }

  object Var {
    def apply(init: Expr[String])(body: Var => Expr[String]) with QuoteContext : Expr[String] = '{
      var x = $init
      ${
        body(
          new Var {
            def get with QuoteContext : Expr[String] = 'x
            def update(e: Expr[String]) with QuoteContext : Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1() with QuoteContext : Expr[String] = Var('{"abc"}) { x =>
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



