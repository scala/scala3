case class Expr[+T](get: T)
trait Ctx[F[_]]
sealed trait Selector[F[_]]:
  def appended(base: Expr[SelectLoop[F]]): Expr[SelectLoop[F]]

 // Without Ctx[F] argument it would compile correctly
class SelectLoop[F[_]](using Ctx[F])
object SelectLoop:
  def loopImpl[F[_]](ctx: Ctx[F])(caseDefs: List[Selector[F]]): Expr[Unit] =
    // Adding explicit type :Expr[SelectLoop[F]] satifies the compiler
    val s0 = Expr(new SelectLoop[F](using ctx))
    val g = caseDefs.foldRight(s0)(_.appended(_))
    Expr(())