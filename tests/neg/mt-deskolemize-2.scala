trait Expr:
  type Value
object Expr:
  type Of[V] = Expr { type Value = V }
  type ExtractValue[F <: Expr] = F match
    case Expr.Of[v] => v
import Expr.ExtractValue

class SimpleLoop1 extends Expr:
  type Value = ExtractValue[SimpleLoop2]

class SimpleLoop2 extends Expr:
  type Value = ExtractValue[SimpleLoop1]

object Test1:
  val x: ExtractValue[SimpleLoop1] = 1 // error

trait Description:
  type Elem <: Tuple

class PrimBroken extends Expr:
  type Value = Alias
  type Alias = Value // error

class Prim extends Expr:
  type Value = BigInt

class VecExpr[E <: Expr] extends Expr:
  type Value = Vector[ExtractValue[E]]

trait ProdExpr extends Expr:
  val description: Description
  type Value = Tuple.Map[description.Elem, [X] =>> ExtractValue[X & Expr]]


class MyExpr1 extends ProdExpr:
  final val description = new Description:
    type Elem = (VecExpr[Prim], MyExpr2)

class MyExpr2 extends ProdExpr:
  final val description = new Description:
    type Elem = (VecExpr[VecExpr[MyExpr1]], Prim)

trait Constable[E <: Expr]:
  def lit(v: ExtractValue[E]): E
object Constable:
  given [E <: Expr]: Constable[E] = ???

object Test2:
  def fromLiteral[E <: Expr : Constable](v: ExtractValue[E]): E =
    summon[Constable[E]].lit(v)
  val x0: ExtractValue[Prim] = "" // error
  val x1: ExtractValue[PrimBroken] = 1 // error

  val foo: MyExpr2 = new MyExpr2
  val v: foo.Value = (Vector(Vector()), 1) // error: Recursion limit exceeded
  val c: MyExpr2 = fromLiteral:
    (Vector(Vector()), 1) // error: Recursion limit exceeded
