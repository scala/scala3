import scala.quoted._

private def impl(x: Expr[Any])(using Quotes): Expr[Unit] = {
  x match
    case '{ foo[x] } =>
      assert(Type.show[x] == "scala.Int", Type.show[x])
    case '{ type f[X]; foo[`f`] } =>
      assert(Type.show[f] == "[A >: scala.Nothing <: scala.Any] =>> scala.collection.immutable.List[A]", Type.show[f])
    case '{ type f <: AnyKind; foo[`f`] } =>
      assert(Type.show[f] == "[K >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] =>> scala.collection.immutable.Map[K, V]", Type.show[f])
    case x => throw MatchError(x.show)
  '{}
}

inline def test(inline x: Any): Unit = ${ impl('x) }

def foo[T <: AnyKind]: Any = ???
