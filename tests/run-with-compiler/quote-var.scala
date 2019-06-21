import scala.quoted._

object Test {

  sealed trait Var {
    def get: Expr[String]
    def update(x: Expr[String]): Expr[Unit]
  }

  object Var {
    def apply(init: Expr[String])(body: Var => Expr[String]): Expr[String] = '{
      var x = $init
      ${
        body(
          new Var {
            def get: Expr[String] = 'x
            def update(e: Expr[String]): Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1(): Expr[String] = Var('{"abc"}) { x =>
    '{
      ${ x.update('{"xyz"}) }
      ${ x.get }
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val res = run {
      test1()
    }
    println(res)
  }
}



