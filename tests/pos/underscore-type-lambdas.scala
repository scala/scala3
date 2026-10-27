//> using options -source 3.11

// `_` is a type lambda placeholder from 3.11 on, without -Xkind-projector
trait Foo[F[_]]
trait Qux[F[_, _]]

class Bar1 extends Foo[Either[Int, _]]
class Bar2 extends Foo[Either[_, Int]]
class Bar3 extends Foo[_ => Int]
class Bar4 extends Foo[(Int, _, Int)]
class Bar5 extends Qux[Map[_, _]]
class Bar6 extends Foo[Either[Int, +_]]

// `*` is not a placeholder without -Xkind-projector
type *[A] = List[A]
class Bar7 extends Foo[*]

// `_` is still a wildcard in type patterns
def f(x: Any) = x match
  case _: List[_] => 1
  case _ => 2
