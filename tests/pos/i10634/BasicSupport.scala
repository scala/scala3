// BasicSupport.scala
trait BasicSupport with
  self: Parser =>

  object SymOps extends SymOps[ctx.type](ctx)
  export SymOps._

  def parse(sym: ctx.Symbol): Int =
    sym.getVisibility()

