import scala.quoted._
object Macro {
  def impl[A](using s: Scope)(using s.Type[A]): Unit = {
    import s.tasty._
    val tpe/*: quoted.Type[? <: AnyKind]*/ = Type.of[A].seal
    '{ f[$tpe] } // error
  }
  def f[T <: AnyKind]: Unit = ()
}
