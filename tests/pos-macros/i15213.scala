import scala.quoted.*

def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])(using Type[T], Quotes): Expr[Unit] = '{}

def sum(arr: Expr[Array[Int]])(using Quotes): Expr[Int] = '{
  var sum = 0
  ${ map(arr, x => '{sum += $x}) }
  sum
}
