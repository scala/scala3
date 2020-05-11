import scala.quoted._
import scala.quoted.staging._

object Test {

  sealed trait Var {
    val outer: Scope
    def get(using s: outer.Nested): s.Expr[String]
    def update(using s: outer.Nested)(x: s.Expr[String]): s.Expr[Unit]
  }

  object Var {
    def apply(using s: Scope)(init: s.Expr[String])(body: (s: Scope) ?=> Var { val outer: s.type } => s.Expr[String]): s.Expr[String] = '{
      var x = $init
      ${
        body(
          new Var {
            val outer: scope.type = scope
            def get(using s: outer.Nested): s.Expr[String] = 'x
            def update(using s: outer.Nested)(e: s.Expr[String]): s.Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1()(using s: Scope): scope.Expr[String] = Var('{"abc"}) { x =>
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



