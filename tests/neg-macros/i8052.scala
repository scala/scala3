import scala.deriving._
import scala.quoted._


object Macro2 {
  trait TC[T] {
    def test(): Unit
  }

  object TC {
    def derived[T](using s: Scope)(ev: s.Expr[Mirror.Of[T]])(using s.Type[T]): s.Expr[TC[T]] = '{
      new TC[T] {
        def encode(): Unit = $ev match {
          case '{ $m: Mirror.ProductOf[T] } => ??? // error
        }
      }
    }
  }
}
