import scala.quoted.*
object Macro {
  inline def ff: Unit = ${impl(Type.of[Int])}
  def impl(t: Type[Int])(using Quotes): Expr[Unit] = '{}
}
