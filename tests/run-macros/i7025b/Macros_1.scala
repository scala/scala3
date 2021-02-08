inline def debug: Unit = ${Macros.debugImpl}

object Macros {
  import scala.quoted._

  def debugImpl(using Quotes): Expr[Unit] = {
    import quotes.reflect._

    def nearestEnclosingDef(owner: Symbol): Symbol =
      if owner.isDefDef then owner
      else if owner.isClassDef then owner
      else nearestEnclosingDef(owner.owner)

    val sym = nearestEnclosingDef(Symbol.spliceOwner)
    if sym.isDefDef then
      val code = sym.signature.toString
      '{ println(${Expr(code)}) }
    else
      '{()}
  }
}
