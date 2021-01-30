
import scala.quoted.*

trait CB[T]

inline def shift : Unit = ${ shiftTerm }

def shiftTerm(using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  val nTree = '{ ??? : CB[Int] }.asTerm
  val tp1 = TypeRepr.of[CB[Int]]
  val tp2 = TypeRepr.of[([X] =>> CB[X])[Int]]
  val ta = Type.of[[X] =>> CB[X]]
  val tp3 = TypeRepr.of(using Type.of[ta.Underlying[Int]])
  val tp4 = TypeRepr.of[CB].appliedTo(TypeRepr.of[Int])
  assert(nTree.tpe <:< tp1)
  assert(nTree.tpe <:< tp2)
  assert(nTree.tpe <:< tp3)
  assert(nTree.tpe <:< tp4)
  '{}
}
