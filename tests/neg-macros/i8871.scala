import scala.quoted._
object Macro {
  def impl[A](using s: Scope)(using s.Type[A]): Unit = {
    import s.tasty._
    val tpe = Type.of[A].seal.asInstanceOf[s.Type[_ <: AnyRef]] // error
    '{ (a: ${tpe}) => ???}
  }
}
