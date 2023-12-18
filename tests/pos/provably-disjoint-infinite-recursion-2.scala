type Tupled[A] <: Tuple = A match
  case Tuple => A & Tuple
  case _     => A *: EmptyTuple

enum Day:
  case Saturday, Sunday

type Foo = Tupled[Day]

def foo(): Foo = Day.Saturday *: EmptyTuple
