/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools
package dotc

import core.Contexts.Context
import reporting.Reporter

/* To do:
 *   - simplify hk types
 *   - have a second look at normalization (leave at method types if pt is method type?)
 *   - Don't open package objects from class files if they are present in source
 *   - Revise the way classes are inherited - when not followed by [...] or (...),
 *     assume the unparameterized type and forward type parameters as we do now for the synthetic head class.
 */
object Bench extends Driver {
  def resident(compiler: Compiler): Reporter = unsupported("resident") /*loop { line =>
    val command = new CompilerCommand(line split "\\s+" toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }*/

  private var numRuns = 1

  def newCompiler(): Compiler = new Compiler

  private def ntimes(n: Int)(op: => Reporter): Reporter =
    (emptyReporter /: (0 until n)) ((_, _) => op)

  override def doCompile(compiler: Compiler, fileNames: List[String], reporter: Option[Reporter] = None)
      (implicit ctx: Context): Reporter =
    if (new config.Settings.Setting.SettingDecorator[Boolean](ctx.base.settings.resident).value(ctx))
      resident(compiler)
    else
      ntimes(numRuns) {
        val start = System.nanoTime()
        val r = super.doCompile(compiler, fileNames, reporter)
        ctx.println(s"time elapsed: ${(System.nanoTime - start) / 1000000}ms")
        r
      }

  def extractNumArg(args: Array[String], name: String, default: Int = 1): (Int, Array[String]) = {
    val pos = args indexOf name
    if (pos < 0) (default, args)
    else (args(pos + 1).toInt, (args take pos) ++ (args drop (pos + 2)))
  }

  override def process(args: Array[String], rootCtx: Context, reporter: Option[Reporter] = None): Reporter = {
    val (numCompilers, args1) = extractNumArg(args, "#compilers")
    val (numRuns, args2) = extractNumArg(args1, "#runs")
    this.numRuns = numRuns
    ntimes(numCompilers)(super.process(args2, rootCtx, reporter))
  }
}


