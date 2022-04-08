package example

type Elem[X] = X match
  case String => Char
  case Array[t] => t
  case Iterable[t] => t

type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match
  case EmptyTuple => Ys
  case x *: xs => x *: Concat[xs, Ys]
