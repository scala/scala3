import reflect.Generic

object Test extends App {
  sealed trait A derives Generic

  object A {
    case class B() extends A
    case class C(x: Int, y: Int) extends A
  }

  println(implicitly[Generic[A]].common.label.deep)
}

