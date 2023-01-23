object Syms:
  import SymDs.*
  opaque type Symbol <: AnyRef
    = SymDenotation
  opaque type ClassSymbol <: Symbol
    = ClassDenotation

object SymDs:
  import Syms.*
  class SymDenotation(sym: Symbol)
  class ClassDenotation(sym: Symbol) extends SymDenotation(sym)

