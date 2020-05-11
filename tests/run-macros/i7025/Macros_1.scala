object Macros {
  import scala.quoted._

  inline def debug: Unit = ${Macros.debugImpl}

  def debugImpl(using s: Scope): s.Expr[Unit] = {
    import s.tasty._

    def nearestEnclosingDef(owner: Symbol): Symbol =
      if owner.isClassDef then owner
      else nearestEnclosingDef(owner.owner)

    val x = nearestEnclosingDef(Symbol.currentOwner)
    if x.isDefDef then
      val code = x.signature.toString
      '{ println(${Expr(code)}) }
    else
      '{()}
  }
}
