import annotation.experimental

@experimental trait Exp
@experimental val exp = 1

type Foo0 = Exp // error
type Foo = Option[Exp] // error
type Bar = Option[exp.type] // error
type Baz = Exp | Int // error
type Quux = [X] =>> X match // error
  case Exp => Int
type Quuz[A <: Exp] = Int // error
