class TypeConstructor {
  type TypeArg
}

trait List[+T] extends TypeConstructor { type TypeArg <: T }

trait Set[T] extends TypeConstructor { type TypeArg <: T }

object obj extends List[Number] with Set[Exception] {
  val x: TypeArg = ???
  val n: Number = x
  val e: Exception = x
}

abstract class Functor[F <: TypeConstructor] {
  def map[A, B](f: F { type TypeArg <: A }): F { type TypeArg <: B }
}

object ListFunctor extends Functor[List] {
  override def map[A, B](f: List { type TypeArg <: A }): List { type TypeArg <: B } = ???
}

