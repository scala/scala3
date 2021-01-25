// Context.scala
trait Context:
  type Symbol

trait SymOps[C <: Context](val c: C):
  extension (sym: c.Symbol)
    def getVisibility(): Int = 0

