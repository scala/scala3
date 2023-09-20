//> using options -experimental

import scala.quoted._

case class T(t: Type[_])

object T {
  def impl[T <: AnyKind](using tt: Type[T])(using Quotes): Expr[Unit] = {
    val t = T(tt)
    t.t match
      case '[type x; x] =>
        assert(Type.show[x] == "scala.Int", Type.show[x])
      case '[type f[X]; f] =>
        assert(Type.show[f] == "[A >: scala.Nothing <: scala.Any] =>> scala.collection.immutable.List[A]", Type.show[f])
      case '[type f[X <: Int]; f] =>
        assert(Type.show[f] == "[T >: scala.Nothing <: scala.Int] =>> C[T]", Type.show[f])
      case '[type f <: AnyKind; f] =>
        assert(Type.show[f] == "[K >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] =>> scala.collection.immutable.Map[K, V]", Type.show[f])
    '{}
  }

  inline def run[T <: AnyKind] = ${ impl[T] }
}
