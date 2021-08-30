import scala.annotation.unchecked.uncheckedVariance

type Untyped = Null

class Type

abstract class Tree[-T >: Untyped] {
  type ThisTree[T >: Untyped] <: Tree[T]

  protected var myTpe: T @uncheckedVariance = _

  def withType(tpe: Type): ThisTree[Type] = {
    val tree = this.asInstanceOf[ThisTree[Type]]
    tree.myTpe = tpe
    tree
  }
}

case class   Ident[-T >: Untyped]() extends Tree[T]
case class  DefDef[-T >: Untyped]() extends Tree[T]
case class Inlined[-T >: Untyped]() extends Tree[T]
case class CaseDef[-T >: Untyped]() extends Tree[T]

def test[T >: Untyped](tree: Tree[T], tp: Type) = tree.withType(tp) match {
  case Ident()       => 1
  case DefDef()      => 2
  case _: Inlined[_] => 3
  case CaseDef()     => 4
  case _             => 5
}
