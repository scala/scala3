import scala.deriving._
import scala.quoted._


object Macro2 {
  trait TC[T] {
    def test(): Unit
  }

  object TC {
    def derived[T: Staged](ev: Expr[Mirror.Of[T]])(using qctx: QuoteContext): Expr[TC[T]] = '{
      new TC[T] {
        def encode(): Unit = $ev match {
          case '{ $m: Mirror.ProductOf[T] } => ??? // error
        }
      }
    }
  }
}
