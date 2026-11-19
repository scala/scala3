package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, SymDenotations.*, StdNames.*, Names.*, Phases.*
import Flags.*, Scopes.*, Decorators.*, NameOps.*, Periods.*, NullOpsDecorator.*
import config.Feature

// Temporary file, see comments on `Definitions.Tuple1`
object SpecializedDefinitions:
  private def withSpecMethods(cls: ClassSymbol, bases: List[Name], paramTypes: Set[TypeRef])(using Context) =
    if !ctx.settings.YcompilingStdlib.value then
      for base <- bases; tp <- paramTypes do
        cls.enter(newSymbol(cls, base.specializedName(List(tp)), Method, ExprType(tp)))
    cls

  def Tuple1(d: Definitions)(using Context): ClassSymbol = withSpecMethods(requiredClass("scala.Tuple1"), List(nme._1), d.Tuple1SpecializedParamTypes)
  def Tuple2(d: Definitions)(using Context): ClassSymbol = withSpecMethods(requiredClass("scala.Tuple2"), List(nme._1, nme._2), d.Tuple2SpecializedParamTypes)
