enum Bool {
  case True
  case False
}

import Bool._

type Not[B <: Bool] = B match {
  case True.type => False.type
  case False.type => True.type
}

val t: True.type = True
val f: False.type = False

val g: Not[False.type] = t
