//> using options -source 3.11 -Xkind-projector:underscores

trait Foo[F[_]]
class Bar1 extends Foo[Either[Int, *]] // warn
class Bar2 extends Foo[λ[x => Either[Int, x]]] // warn
class Bar3 extends Foo[Either[Int, _]]
