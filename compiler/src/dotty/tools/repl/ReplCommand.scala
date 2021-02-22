package dotty.tools.repl

import dotty.tools.dotc.config.Properties._
import dotty.tools.dotc.config.CompilerCommand

object ReplCommand extends CompilerCommand:
  override def cmdName: String = "scala"
  override def versionMsg: String = s"Scala code runner $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scala -help  gives more information"
