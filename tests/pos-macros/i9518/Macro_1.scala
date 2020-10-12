
import scala.quoted._

trait CB[T]

inline def shift : Unit = ${ shiftTerm }

def shiftTerm(using QuoteContext): Expr[Unit] = {
  import qctx.reflect._
  val nTree = '{ ??? : CB[Int] }.unseal
  val tp1 = '[CB[Int]].unseal.tpe
  val tp2 = '[([X] =>> CB[X])[Int]].unseal.tpe
  val ta = '[[X] =>> CB[X]]
  val tp3 = '[ta.T[Int]].unseal.tpe
  val tp4 = '[CB].unseal.tpe.appliedTo(Type.of[Int])
  assert(nTree.tpe <:< tp1)
  assert(nTree.tpe <:< tp2)
  assert(nTree.tpe <:< tp3)
  assert(nTree.tpe <:< tp4)
  '{}
}
