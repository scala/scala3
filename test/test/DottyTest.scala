package test

import dotty.compiler.internal._
import core._
import core.Contexts._
import core.Symbols._
import core.Flags._
import Types._, Symbols._, Decorators._
import printing.Texts._
import reporting.ConsoleReporter
import core.Decorators._

class DottyTest {

  parsing.Scanners // initialize keywords

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
