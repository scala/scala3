object Test:
  val x  = summon[Lie.TC[Lie[7]]]
  val fails = summon[x.Out =:= 7]
