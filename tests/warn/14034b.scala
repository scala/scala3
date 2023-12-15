//> using options  -deprecation

@deprecated trait Exp
@deprecated val exp = 1

def test1 = exp // warn
def test2(a: Exp) = () // warn

type Foo0 = Exp // warn
type Foo = Option[Exp] // warn
type Bar = Option[exp.type] // warn
type Baz = Exp | Int // warn
type Quux = [X] =>> X match
  case Exp => Int // warn
type Quuz[A <: Exp] = Int // warn