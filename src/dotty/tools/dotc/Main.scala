/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools
package dotc

import core.Contexts.Context
import reporting.Reporter

/* To do:
 */
object Main extends Driver {
  def resident(compiler: Compiler): Reporter = unsupported("resident") /*loop { line =>
    val command = new CompilerCommand(line split "\\s+" toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }*/

  override def newCompiler(): Compiler = new Compiler

  override def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter = {
    if (new config.Settings.Setting.SettingDecorator[Boolean](ctx.base.settings.resident).value(ctx))
      resident(compiler)
    else
      super.doCompile(compiler, fileNames)
  }
}
