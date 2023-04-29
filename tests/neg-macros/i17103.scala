import scala.quoted.*

def test(using Quotes): Expr[Unit] =
  '{
    trait C:
      def d: Int
    val c: C = ???
    ${
      val expr = '{
        val cRef: c.type = ???
        cRef.d // error
        ()
      }
      expr
    }
  }