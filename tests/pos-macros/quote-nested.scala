import scala.annotation.tailrec
import scala.quoted._

object Macro {

  inline def foo: Unit = ${ nested() }

  private def nested(using s: Scope)(): s.Expr[Unit] = '{
    var i = 0
    ${
      val x: scope.Expr[Double] = '{
        val y: Boolean = ${
          val z: scope.Expr[Int] = '{i + 3}
          '{true}
        }
        4.2
      }
      '{}
    }
    ()
  }
}
