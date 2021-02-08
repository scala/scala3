class TreeAccumulator2 {

  def foo(reflect: Reflection2)(tree: Any): Unit = {
    import reflect.*
    tree match {
      case A() =>
      case B() =>
      case C() =>
      case D() =>
    }
  }

}

abstract class Reflection2 {

  type X
  type Y

  implicit def xct: scala.reflect.ClassTag[X]
  implicit def yct: scala.reflect.ClassTag[Y]

  val A: AExtractor
  trait AExtractor {
    def unapply(x: X): Boolean
  }

  val B: BExtractor
  trait BExtractor {
    def unapply(x: X): Boolean
  }

  val C: CExtractor
  trait CExtractor {
    def unapply(x: Y): Boolean // Note the type Y
  }

  val D: DExtractor
  trait DExtractor {
    def unapply(x: X): Boolean
  }

}
