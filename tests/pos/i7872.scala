object Test {
  def flip: (x: 0 | 1) => x.type match { case 0 => 1 case 1 => 0 } = ???
  flip(0): 1
  flip(1): 0
  flip(if ??? then 0 else 1)
  val n: 0 | 1 = if ??? then 0 else 1
  flip(n)
  val m: n.type match { case 0 => 1 case 1 => 0 } = flip(n)
}

object Test2 {
  type Flip[N <: 0 | 1] <: 0 | 1 = N match { case 0 => 1 case 1 => 0 }
  def flip: (x: 0 | 1) => Flip[x.type] = ???
  flip(0): 1
  flip(1): 0
}

object Test3 {
  type Flip[N <: 0 | 1] <: 0 | 1 = N match { case 0 => 1 case 1 => 0 }
  def flip(x: 0 | 1): Flip[x.type] = ???
  flip(0): 1
  flip(1): 0
}
