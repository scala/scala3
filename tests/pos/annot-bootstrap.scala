package scala.annotation.unchecked

// note: if the case class is not inside an object, the error disappears
object test {
  case class L(a: Int)
}

final class uncheckedVariance extends scala.annotation.StaticAnnotation {}
