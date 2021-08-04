package kind_projector_neg

trait Foo[F[_]]

class Bar1 extends Foo[Either[_, _]] // error
class Bar2 extends Foo[_] // error
class Bar3 extends Foo[λ[List[x] => Int]] // error

object Test {
  type -_ = Int // error -_ not allowed as a type def name without backticks
  type +_ = Int // error +_ not allowed as a type def name without backticks
}

class BacktickUnderscoreIsNotFine extends Foo[List[`_`]] // error wildcard invalid as backquoted identifier
