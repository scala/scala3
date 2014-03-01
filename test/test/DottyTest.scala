package test

import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.reporting.ConsoleReporter
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.Compiler

import dotty.tools.dotc
import dotty.tools.dotc.core.Phases.Phase

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

  def checkCompile(checkAfterPhase: String, source:String)(assertion:tpd.Tree =>Unit): Unit = {
    val c = new Compiler {
      override def phases = {
        val allPhases = super.phases
        val targetPhase = allPhases.find{p=> p.name == checkAfterPhase}
        assert(targetPhase isDefined)
        val phasesBefore = allPhases.takeWhile(x=> ! (x eq targetPhase.get))

        val checker = new Phase{
          def name = "assertionChecker"
          override def run(implicit ctx: Context): Unit = assertion(ctx.compilationUnit.tpdTree)
        }
        phasesBefore:::List(targetPhase.get, checker)
      }
    }
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(source)
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
