// Context.scala
trait Context with
  type Symbol

trait SymOps[C <: Context](val c: C) with
  extension (sym: c.Symbol)
    def getVisibility(): Int = 0

