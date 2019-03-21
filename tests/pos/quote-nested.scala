import scala.annotation.tailrec
import scala.quoted._

object Macro {

  inline def foo: Unit = ${ nested() }

  private def nested(): Expr[Unit] = '{
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
