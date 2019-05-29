import scala.quoted._
import scala.tasty._


inline def isFunctionType[T:Type]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(implicit refl: Reflection): Expr[Boolean] = {
  import refl._
  tp.unseal.tpe.isFunctionType.toExpr
}
