
def foo(x: Option[(Int, Boolean)]) = x match
  case Some(a, b) => ??? // was ok
  case None => ???

def bar(x: Option[Int *: Boolean *: EmptyTuple]) = x match
  case Some(a, b) => ??? // was error, now ok
  case None => ???
