enum Bool {
  case True
  case False

  // just to make sure we are using reference equality
  override def equals(a: Any) = false

}

import Bool.*

type Not[B <: Bool] = B match {
  case True.type => False.type
  case False.type => True.type
}

def not[B <: Bool & Singleton](b: B): Not[B] = b match {
  case b: True.type => False
  case b: False.type => True
}

@main def Test =

  val t: True.type = True
  val f: False.type = False

  val t1: Not[False.type] = t
  val f1: Not[True.type] = f

  assert(not(True).asInstanceOf[AnyRef] eq False)
  assert(not(False).asInstanceOf[AnyRef] eq True)
