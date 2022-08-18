type Var[TV]
type Set[TS, D <: Var[TS], V <: TS]
type Foo[FX <: GX, GX]

type Get[S] = S match {
  case Set[t, d, v] => v
}

def get(s: Any) = s match
  case _: Set[t, d, v] => ??? : v
  case _: Foo[a, b] => ???
