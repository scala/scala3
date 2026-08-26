trait Iterable[+A]

enum Expr {
  case Case[T, C <: Iterable[T]]()
}
object Expr {
  val m = summon[scala.deriving.Mirror.SumOf[Expr]]
  summon[m.MirroredElemTypes =:= Expr.Case[Any, Iterable[Any]] *: EmptyTuple]
}

enum Trait {
  case Upcast[Base, Case <: Base]()
}

object Trait {
  val m = summon[scala.deriving.Mirror.SumOf[Trait]]
  summon[m.MirroredElemTypes =:= Trait.Upcast[Any, Any] *: EmptyTuple]
}

enum MoreDeps {
  case Case[T, C <: Iterable[T], D <: Iterable[C]]()
}
object MoreDeps {
  val m = summon[scala.deriving.Mirror.SumOf[MoreDeps]]
  summon[m.MirroredElemTypes =:= MoreDeps.Case[Any, Iterable[Any], Iterable[Iterable[Any]]] *: EmptyTuple]
}

enum Circular {
  case Case[C <: Iterable[D], D <: C]()
}
object Circular {
  val m = summon[scala.deriving.Mirror.SumOf[Circular]]
  summon[m.MirroredElemTypes =:= Circular.Case[Iterable[Nothing], Nothing] *: EmptyTuple]
}

