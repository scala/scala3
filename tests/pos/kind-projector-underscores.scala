//> using options -Xkind-projector:underscores

package kind_projector

trait Foo[F[_]]
trait Qux[F[_, _]]
trait Baz[F[_], A, B]

trait FooPlus[+F[+_]]
trait QuxPlus[+F[+_, +_]]
trait BazPlus[+F[+_], +A, +B]

trait FooMinus[-F[-_]]
trait QuxMinus[-F[-_, -_]]
trait BazMinus[-F[-_], -A, -B]

class Bar1 extends Foo[Either[Int, _]]
class Bar2 extends Foo[Either[_, Int]]
class Bar3 extends Foo[_ => Int]
class Bar4 extends Foo[Int => _]
class Bar5 extends Foo[(Int, _, Int)]
class Bar6 extends Foo[λ[x => Either[Int, x]]]
class Bar7 extends Qux[λ[(x, y) => Either[y, x]]]
class Bar8 extends Foo[Baz[Int => _, _, Int]]
class Bar9 extends Foo[λ[x => Baz[x => _, Int, x]]]

class BarPlus1 extends FooPlus[Either[Int, +_]]
class BarPlus2 extends FooPlus[Either[+_, Int]]
class BarPlus3 extends FooPlus[Int => +_]
class BarPlus4 extends FooPlus[(Int, +_, Int)]
class BarPlus5 extends FooPlus[λ[`+x` => Either[Int, x]]]
class BarPlus6 extends QuxPlus[λ[(`+x`, `+y`) => Either[y, x]]]
class BarPlus7 extends FooPlus[BazPlus[Int => +_, +_, Int]]

class BarMinus1 extends FooMinus[-_ => Int]

class VarianceAnnotationIsActuallyIgnored1 extends FooPlus[Either[Int, -_]]
class VarianceAnnotationIsActuallyIgnored2 extends FooPlus[Either[-_, Int]]
class VarianceAnnotationIsActuallyIgnored3 extends FooMinus[+_ => Int]
class VarianceAnnotationIsActuallyIgnored4 extends FooPlus[Int => -_]
class VarianceAnnotationIsActuallyIgnored5 extends FooPlus[(Int, -_, Int)]
class VarianceAnnotationIsActuallyIgnored6 extends FooPlus[λ[`-x` => Either[Int, x]]]
class VarianceAnnotationIsActuallyIgnored7 extends QuxPlus[λ[(`-x`, `-y`) => Either[y, x]]]
class VarianceAnnotationIsActuallyIgnored8 extends FooPlus[BazPlus[Int => -_, -_, Int]]
class VarianceAnnotationIsActuallyIgnored9 extends Foo[λ[`-x` => BazPlus[x => -_, Int, x]]]

class BackticksAreFine1 extends FooPlus[Either[Int, `-_`]]
class BackticksAreFine2 extends FooPlus[Either[`-_`, Int]]
class BackticksAreFine3 extends FooMinus[`+_` => Int]
class BackticksAreFine4 extends FooPlus[Int => `-_`]
class BackticksAreFine5 extends FooPlus[(Int, `-_`, Int)]
class BackticksAreFine6 extends FooPlus[BazPlus[Int => `-_`, `-_`, Int]]
class BackticksAreFine7 extends Foo[λ[`-x` => BazPlus[x => `-_`, Int, x]]]

class SpacesAreFine1 extends FooPlus[Either[Int, -  _  ]]
class SpacesAreFine2 extends FooPlus[Either[ - _ , Int]]
class SpacesAreFine3 extends FooMinus[ + _ => Int]
class SpacesAreFine4 extends FooPlus[Int => - _]
class SpacesAreFine5 extends FooPlus[(Int, - _, Int)]
class SpacesAreFine6 extends FooPlus[BazPlus[Int => -  _ ,  - _, Int]]
class SpacesAreFine7 extends Foo[λ[`-x` => BazPlus[x =>  - _ , Int, x]]]
