object Macros {
  import scala.quoted._

  inline def debug: Unit = ${Macros.debugImpl}

  def debugImpl with (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{given, _}

    def nearestEnclosingDef(owner: Symbol): Symbol =
      if owner.isClassDef then owner
      else nearestEnclosingDef(owner.owner)

    val x = nearestEnclosingDef(rootContext.owner)
    if x.isDefDef then
      val code = x.signature.toString
      '{ println(${Expr(code)}) }
    else
      '{()}
  }
}
