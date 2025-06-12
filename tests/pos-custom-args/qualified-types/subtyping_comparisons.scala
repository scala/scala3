def tp[T](): Boolean = ???

class Outer:
  class Inner:
    class D
    summon[{v: Boolean with tp[Inner.this.D]()} =:= {v: Boolean with tp[D]()}]

object OuterO:
  object InnerO:
    class D
    summon[{v: Boolean with tp[InnerO.this.D]()} =:= {v: Boolean with tp[D]()}]
    summon[{v: Boolean with tp[InnerO.D]()} =:= {v: Boolean with tp[D]()}]
    summon[{v: Boolean with tp[OuterO.InnerO.D]()} =:= {v: Boolean with tp[D]()}]
