package test

import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotty.tools.dotc.util.Texts._
import dotty.tools.dotc.core.Decorators._
import org.junit.Test

class DottyTest {

  implicit val ctx: Context = {
    val base = Context.theBase
    import base.settings._
    val ctx = base.initialCtx.fresh
      .withSetting(verbose, true)
      .withSetting(debug, true)
      .withSetting(debugTrace, true)
      .withSetting(Ylogcp, true)
      .withSetting(printtypes, true)
      .withSetting(pageWidth, 90)
      .withSetting(log, List("<some"))
    println(ctx.settings)
    base.definitions.init()
    ctx
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
