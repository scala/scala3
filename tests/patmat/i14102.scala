trait T[-X]
case class CC[-X](x: List[T[X]]) extends T[Nothing]
case class Id[-X](x: String) extends T[X]

def f[X](tree: T[X]) = tree match
  case CC(Id("hi") :: Nil) => ???
  case CC(refs) => ???
  case _ => ???
