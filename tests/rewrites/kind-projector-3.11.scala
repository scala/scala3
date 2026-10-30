trait Foo[F[_]]
trait Baz[F[_], A, B]

// `_` is still a wildcard here, so `*` is not rewritten
class Bar1 extends Foo[Map[_, *]]
class Bar2 extends Foo[λ[x => Map[_, x]]]
class Bar3 extends Foo[λ[x => Baz[x => *, Int, x]]]
