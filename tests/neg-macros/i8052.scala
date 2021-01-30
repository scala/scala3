import scala.deriving.*
import scala.quoted.*


object Macro2 {
  trait TC[T] {
    def test(): Unit
  }

  object TC {
    def derived[T: Type](ev: Expr[Mirror.Of[T]])(using Quotes): Expr[TC[T]] = '{
      new TC[T] {
        def encode(): Unit = $ev match {
          case '{ $m: Mirror.ProductOf[T] } => ??? // error
        }
      }
    }
  }
}
