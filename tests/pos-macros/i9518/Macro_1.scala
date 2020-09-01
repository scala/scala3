
import scala.quoted._

trait CB[T]

inline def shift : Unit = ${ shiftTerm }

def shiftTerm(using QuoteContext): Expr[Unit] = {
  import qctx.tasty._
  val nTree = '{ ??? : CB[Int] }.unseal
  val tp1 = quoted.Type[CB[Int]].unseal.tpe
  val tp2 = quoted.Type[([X] =>> CB[X])[Int]].unseal.tpe
  val ta = quoted.Type[[X] =>> CB[X]]
  val tp3 = quoted.Type[ta.T[Int]].unseal.tpe
  val tp4 = quoted.Type[CB].unseal.tpe.appliedTo(typeOf[Int])
  assert(nTree.tpe <:< tp1)
  assert(nTree.tpe <:< tp2)
  assert(nTree.tpe <:< tp3)
  assert(nTree.tpe <:< tp4)
  '{}
}
