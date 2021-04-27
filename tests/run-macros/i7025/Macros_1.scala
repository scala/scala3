object Macros {
  import scala.quoted.*

  inline def debug: Unit = ${Macros.debugImpl}

  def debugImpl(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    def nearestEnclosingDef(owner: Symbol): Symbol =
      if owner.isClassDef then owner
      else nearestEnclosingDef(owner.owner)

    val x = nearestEnclosingDef(Symbol.spliceOwner)
    if x.isDefDef then
      val code = x.signature.toString
      '{ println(${Expr(code)}) }
    else
      '{()}
  }
}
