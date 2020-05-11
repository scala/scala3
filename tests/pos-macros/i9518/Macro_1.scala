
import scala.quoted._

trait CB[T]

inline def shift : Unit = ${ shiftTerm }

def shiftTerm(using Scope): scope.Expr[Unit] = {
  import scope.tasty._
  val nTree = '{ ??? : CB[Int] }
  val tp1 = '[CB[Int]].tpe
  val tp2 = '[([X] =>> CB[X])[Int]].tpe
  val ta = '[[X] =>> CB[X]]
  val tp3 = '[ta.X[Int]].tpe
  val tp4 = '[CB].tpe.appliedTo(Type.of[Int])
  assert(nTree.tpe <:< tp1)
  assert(nTree.tpe <:< tp2)
  assert(nTree.tpe <:< tp3)
  assert(nTree.tpe <:< tp4)
  '{}
}
