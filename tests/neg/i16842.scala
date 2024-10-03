sealed trait Expr1
sealed trait Literal extends Expr1

case class ArrayLiter(elems: List[Expr1]) extends Literal

sealed trait SemanticType {
  type T // the type with which a literal of this semanticType is represented
}
case object SemanticInt extends SemanticType {
  type T = Int
}

case class SemanticArray[U <: SemanticType](dim: Int) extends SemanticType {
  type T = List[U]
}

sealed trait Expr2[+T]
class Liter[T <: SemanticType](val ty: T, val value: ty.T) extends Expr2[T]

def typecheckArrayLiter(
    a: ArrayLiter
): Liter[SemanticArray[SemanticType]] = {
  val x: List[Expr2[SemanticInt.type]] = List()
  Liter(SemanticArray[SemanticInt.type], x) // error // error
}
