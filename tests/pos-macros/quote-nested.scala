import scala.annotation.tailrec
import scala.quoted.*

object Macro {

  inline def foo: Unit = ${ nested() }

  private def nested()(using Quotes): Expr[Unit] = '{
    var i = 0
    ${
      val x: Expr[Double] = '{
        val y: Boolean = ${
          val z: Expr[Int] = '{i + 3}
          '{true}
        }
        4.2
      }
      '{}
    }
    ()
  }
}
