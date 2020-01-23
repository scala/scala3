package kind_projector

trait Foo[F[_]]
trait Qux[F[_, _]]
trait Baz[F[_], A, B]

class Bar1 extends Foo[Either[Int, *]]
class Bar2 extends Foo[Either[*, Int]]
class Bar3 extends Foo[* => Int]
class Bar4 extends Foo[Int => *]
class Bar5 extends Foo[(Int, *, Int)]
class Bar6 extends Foo[λ[x => Either[Int, x]]]
class Bar7 extends Qux[λ[(x, y) => Either[y, x]]]
class Bar8 extends Foo[Baz[Int => *, *, Int]]
class Bar9 extends Foo[λ[x => Baz[x => *, Int, x]]]
