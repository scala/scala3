import scala.quoted.*

object Macro:
  inline def generateCode: Unit = ${ testThisPaths }

  def testThisPaths(using Quotes): Expr[Unit] =
    '{
      trait E extends G:
        type V
        val f: F
        ${
          val expr = '{
            // val _: Any = this // FIXME: this should work
            // val _: Any = f // FIXME: this should work
            val _: this.type = ???
            val _: V = ???
            val _: this.V = ???
            val _: this.f.V = ???
            val _: this.type = ???
            val _: this.f.type = ???
          }
          expr
        }
      trait F:
        type V
    }

trait G:
  val f: Any
