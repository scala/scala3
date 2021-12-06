
@deprecated trait Exp
@deprecated val exp = 1

def test1 = exp // error
def test2(a: Exp) = () // error

type Foo0 = Exp // error
type Foo = Option[Exp] // error
type Bar = Option[exp.type] // error
type Baz = Exp | Int // error
type Quux = [X] =>> X match // error
  case Exp => Int
type Quuz[A <: Exp] = Int // error
