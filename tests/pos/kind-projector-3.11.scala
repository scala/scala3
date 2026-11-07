//> using options -source 3.11 -Xkind-projector

// `_` is not a placeholder here, so `*` is not deprecated
trait Foo[F[_]]
class Bar1 extends Foo[Either[Int, *]]
