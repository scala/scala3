/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools
package dotc

import core.Contexts.Context
import config.Settings.Setting.*

object Main extends Driver {
  def resident(compiler: Compiler): Unit = unsupported("resident") /*loop { line =>
    val command = new CompilerCommand(line split "\\s+" toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }*/

  override def newCompiler(): Compiler = new Compiler

  override def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Unit = {
    if (ctx.base.settings.resident.value) resident(compiler) // error
    else super.doCompile(compiler,123) // error: type mismatch
  }
}
