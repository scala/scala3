enum Bool {
  case True
  case False
}

import Bool.*

type Not[B <: Bool] = B match {
  case True.type => False.type
  case False.type => True.type
}

val t: True.type = True
val f: False.type = False

val g: Not[False.type] = t

val t1: Not[f.type] = t // transitivity
val f1: Not[t.type] = f // transitivity

val t2: Not[f1.type] = t1 // transitivity x2
val f2: Not[t1.type] = f1 // transitivity x2
