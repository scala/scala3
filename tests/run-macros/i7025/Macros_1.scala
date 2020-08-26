object Macros {
  import scala.quoted._

  inline def debug: Unit = ${Macros.debugImpl}

  def debugImpl(using qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

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
