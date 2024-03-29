object Test:

  def flip: (x: 0 | 1) => x.type match { case 0 => 1 case 1 => 0 } = ???

  flip(0): 1
  flip(1): 0

  flip(if ??? then 0 else 1)
  val n: 0 | 1 = if ??? then 0 else 1
  flip(n)

  val m: n.type match { case 0 => 1 case 1 => 0 } = flip(n)

  // The following do not work, see discussion in https://github.com/scala/scala3/pull/7835/files/6e60814e69be5c8d60265d4ce4bc1758863c23d8#r361741296:
  // flip(m)
  // flip(flip(n))
