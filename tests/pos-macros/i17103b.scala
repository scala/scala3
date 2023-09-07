import scala.quoted.*

trait C0:
  def d: Int

def test(using Quotes): Expr[Unit] =
  '{
    trait C1 extends C0:
      def d: Int
    trait C extends C1:
      def d: Int
    val c: C = ???
    ${
      val expr = '{
        val cRef: c.type = ???
        cRef.d // calls C0.d
        ()
      }
      expr
    }
  }
