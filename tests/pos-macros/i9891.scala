import scala.quoted._
import scala.deriving.Mirror
def derived[T: Type](using Quotes): Expr[Any] = {
  val ev1: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get
  val ev2: Expr[Mirror.Of[T]] = Expr.summon(using Type.of[Mirror.Of[T]]).get
  '{null}
}
