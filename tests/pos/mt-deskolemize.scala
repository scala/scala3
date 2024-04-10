trait Expr:
  type Value

object Expr:
  type Of[V] = Expr { type Value = V }
  type ExtractValue[F <: Expr] = F match
    case Expr.Of[v] => v
import Expr.ExtractValue

class Prim extends Expr:
  type Value = Alias
  type Alias = BigInt

class VecExpr[E <: Expr] extends Expr:
  type Value = Vector[ExtractValue[E]]

trait Description:
  type Elem <: Tuple

trait ProdExpr extends Expr:
  val description: Description
  type Value = Tuple.Map[description.Elem, [X] =>> ExtractValue[X & Expr]]

class MyExpr1 extends ProdExpr:
  final val description = new Description:
    type Elem = (VecExpr[Prim], Prim)

class MyExpr2 extends ProdExpr:
  final val description = new Description:
    type Elem = (VecExpr[VecExpr[MyExpr1]], Prim)

trait ProdExprAlt[T <: Tuple] extends Expr:
  type Value = Tuple.Map[T, [X] =>> ExtractValue[X & Expr]]

class MyExpr3 extends ProdExprAlt[(Prim, VecExpr[Prim], Prim)]

trait Constable[E <: Expr]:
  def lit(v: ExtractValue[E]): E
object Constable:
  given [E <: Expr]: Constable[E] = ???

object Test:
  def fromLiteral[E <: Expr : Constable](v: ExtractValue[E]): E =
    summon[Constable[E]].lit(v)
  val a: Prim = fromLiteral(1)
  val b: VecExpr[Prim] = fromLiteral(Vector(1))
  val c: MyExpr1 = fromLiteral((Vector(1), 1))
  val d: MyExpr2 = fromLiteral(Vector(Vector((Vector(1), 1))), 2)
  val e: MyExpr3 = fromLiteral((1, Vector(1), 1))
  val f: ProdExprAlt[(MyExpr1, VecExpr[MyExpr3])] = fromLiteral:
    (
      (Vector(1), 1),
      Vector((1, Vector(1), 1), (2, Vector(1), 2))
    )
  val g: Expr { type Alias = Int; type Value = Alias } = fromLiteral(1)
