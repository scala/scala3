trait First {type Out}
given First {type Out = 123}

trait Second {type Out}
transparent inline given (using f: First): Second = new Second {type Out = f.Out}

val s = summon[Second]
val x = summon[s.Out =:= 123]