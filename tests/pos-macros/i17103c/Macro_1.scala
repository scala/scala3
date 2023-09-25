import scala.quoted.*

inline def test = ${ testExpr }

def testExpr(using Quotes): Expr[Unit] =
  '{
    trait C
    val c: C = ???
    ${
      val expr = '{
        val cRef: c.type = ???
        ()
      }
      expr
    }
  }
