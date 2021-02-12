enum Bool {
  case True
  case False
}

import Bool.*

type Not[B <: Bool] = B match {
  case True.type => False.type
  case False.type => True.type
}

def not[B <: Bool & Singleton](b: B): Not[B] = b match {
  case b: False.type => True // error
  case b: True.type => False // error
}

val f: Not[False.type] = False // error: Found: (Bool.False : Bool) Required: (Bool.True : Bool)
