class T
class A extends T
class B extends T

def test(tp: T) =
  val mapping: Map[A, String] = ???

  tp match
    case a: A => mapping(a) match
      case s: String => B()
      case null => a
