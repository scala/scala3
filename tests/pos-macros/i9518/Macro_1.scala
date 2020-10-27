
import scala.quoted._

trait CB[T]

inline def shift : Unit = ${ shiftTerm }

def shiftTerm(using QuoteContext): Expr[Unit] = {
  import qctx.reflect._
  val nTree = '{ ??? : CB[Int] }.unseal
  val tp1 = Type[CB[Int]].unseal.tpe
  val tp2 = Type[([X] =>> CB[X])[Int]].unseal.tpe
  val ta = Type[[X] =>> CB[X]]
  val tp3 = Type[ta.Underlying[Int]].unseal.tpe
  val tp4 = Type[CB].unseal.tpe.appliedTo(TypeRepr.of[Int])
  assert(nTree.tpe <:< tp1)
  assert(nTree.tpe <:< tp2)
  assert(nTree.tpe <:< tp3)
  assert(nTree.tpe <:< tp4)
  '{}
}
