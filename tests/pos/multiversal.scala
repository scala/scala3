object Test {
  import scala.CanEqual

  given [X, Y] => CanEqual[X, Y] => CanEqual[List[X], List[Y]] = CanEqual.derived

  val b: Byte = 1
  val c: Char = 2
  val i: Int = 3
  val l: Long = 4L
  val ii: Integer = i

  List(b) == List(l)
  List(l) == List(c)
  List(b) != List(c)
  List(i) == List(l)
  List(i) == List(ii)
  List(ii) == List(l)
  List(b) == List(ii)
  List(ii) == List(l)

  import reflect.ClassTag
  val BooleanTag: ClassTag[Boolean]      = ClassTag.Boolean

  class Setting[T: ClassTag] {
    def doSet() = implicitly[ClassTag[T]] match {
      case BooleanTag =>
      case _ =>
    }
  }
}