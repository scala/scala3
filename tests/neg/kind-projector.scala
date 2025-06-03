//> using options -Xkind-projector

package kind_projector_neg

trait Foo[F[_]]

class Bar1 extends Foo[Either[*, *]] // error
class Bar2 extends Foo[*] // error
class Bar3 extends Foo[λ[List[x] => Int]] // error
