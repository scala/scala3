import scala.deriving.Mirror
import scala.compiletime.{constValue, error}
import scala.quoted.*

object TestMacro {
  inline def test1: Unit =
    ${ code() }

  def code()(using Quotes): Expr[Unit] =
    '{
      println(${Expr(Type.valueOfTuple[EmptyTuple].toString)})
      println(${Expr(Type.valueOfTuple[1 *: EmptyTuple].toString)})
      println(${Expr(Type.valueOfTuple[(1, 2)].toString)})
      println(${Expr(Type.valueOfTuple[(1, 2, 3)].toString)})
      println(${Expr(Type.valueOfTuple[(1, 2, 3, 4)].toString)})
      println(${Expr(Type.valueOfTuple[(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)].toString)})
    }
}
