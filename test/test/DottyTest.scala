package test

import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.reporting.ConsoleReporter
import dotty.tools.dotc.core.Decorators._

class DottyTest {

  dotty.tools.dotc.parsing.Scanners // initialize keywords

  implicit val ctx: Context = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
      .withSetting(verbose, true)
//      .withSetting(debug, true)
//      .withSetting(debugTrace, true)
//      .withSetting(prompt, true)
      .withSetting(Ylogcp, true)
      .withSetting(printtypes, true)
      .withSetting(pageWidth, 90)
      .withSetting(log, List("<some"))
 //   .withTyperState(new TyperState(new ConsoleReporter()(base.initialCtx)))

//      .withSetting(uniqid, true)
    println(ctx.settings)
    base.definitions.init(ctx)
    ctx
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
