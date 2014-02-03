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

  lazy val compiler = new Compiler

  override def newCompiler(): Compiler = compiler

  override def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter = {
    if (new config.Settings.Setting.SettingDecorator[Boolean](ctx.base.settings.resident).value(ctx))
      resident(compiler)
    else
      super.doCompile(compiler, fileNames)
  }

  val N = 10

  override def main(args: Array[String]): Unit = {
    for (i <- 0 until N) {
      val start = System.nanoTime()
      process(args)
      println(s"time elapsed: ${(System.nanoTime - start) / 1000000}ms")
    }
  }
}


