def tp[T](): Boolean = ???

class Outer:
  class Inner:
    type D
    summon[{v: Boolean with tp[Inner.this.D]()} =:= {v: Boolean with tp[D]()}]

object OuterO:
  object InnerO:
    type D
    summon[{v: Boolean with tp[InnerO.this.D]()} =:= {v: Boolean with tp[D]()}]

    // Before normalization:
    // lhs: <empty>.this.OuterO$.this.InnerO$.this.D
    // rhs: <empty>.this.OuterO$.this.InnerO.D
    summon[{v: Boolean with tp[InnerO.D]()} =:= {v: Boolean with tp[D]()}]

    summon[{v: Boolean with tp[OuterO.InnerO.D]()} =:= {v: Boolean with tp[D]()}]
