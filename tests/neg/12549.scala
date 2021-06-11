enum Bool {
  case True
  case False
}

import Bool.*

type Not[B <: Bool] = B match {
  case True.type => False.type
  case False.type => True.type
  case _ => "unreachable"
}

def foo[B <: Bool & Singleton]: Unit = {
  implicitly[Not[B] =:= "unreachable"] // error

  ()
}
