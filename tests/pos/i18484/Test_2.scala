class Test {
  def test(): Unit = {
    scala.compiletime.testing.typeCheckErrors("Macro.subtypes[top.inner.Inner.Shape]")
  }
}

package top {
  package inner {
    object Inner:
      sealed trait Shape
      case class Circle(center: Int, rad: Double) extends Shape
      object Inner2:
        case class Circle2(center: Int, rad: Double) extends Shape
  }
  case class Circle3(center: Int, rad: Double) extends inner.Inner.Shape
}
case class Circle4(center: Int, rad: Double) extends top.inner.Inner.Shape

package top2 {
  case class Circle5(center: Int, rad: Double) extends top.inner.Inner.Shape
}
