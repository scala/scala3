import scala.reflect.ClassTag

class Test:
  type A

  given ClassTag[A] = ???

  var a: A | Null = null

  a match { //WARNING: match may not be exhaustive. It would fail on pattern case: _: A
    case null =>
    case a: A =>
  }
