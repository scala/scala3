package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, SymDenotations.*, StdNames.*, Names.*, Phases.*
import Flags.*, Scopes.*, Decorators.*, NameOps.*, Periods.*, NullOpsDecorator.*

// Temporary file, see comments on `Definitions.Tuple1`
object SpecializedDefinitions:
  def Tuple1(d: Definitions)(using Context): ClassSymbol = requiredClass("scala.Tuple1")
  def Tuple2(d: Definitions)(using Context): ClassSymbol = requiredClass("scala.Tuple2")
