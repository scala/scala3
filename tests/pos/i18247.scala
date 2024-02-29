sealed trait Op
object Op {
  case object `==` extends Op
}

def t1(a: Op): true = {
  a match {
    case Op.`==` => true // was: won't compile
  }
}
