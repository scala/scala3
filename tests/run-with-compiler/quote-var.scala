import scala.quoted._

object Test {

  sealed trait Var {
    def get: Staged[String]
    def update(x: Expr[String]): Staged[Unit]
  }

  object Var {
    def apply(init: Expr[String])(body: Var => Expr[String]): Staged[String] = '{
      var x = $init
      ${
        body(
          new Var {
            def get: Staged[String] = 'x
            def update(e: Expr[String]): Staged[Unit] = '{ x = $e }
          }
        )
      }
    }
  }


  def test1(): Staged[String] = Var('{"abc"}) { x =>
    '{
      ${ x.update('{"xyz"}) }
      ${ x.get }
    }
  }

  def main(args: Array[String]): Unit = {
    val tb: Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    println(tb.run(test1()))
  }
}



