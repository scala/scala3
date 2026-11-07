//> using options -source future -Xkind-projector:underscores

trait Foo[F[_]]
class Bar1 extends Foo[Either[Int, *]] // error
class Bar2 extends Foo[λ[x => Either[Int, x]]] // error
