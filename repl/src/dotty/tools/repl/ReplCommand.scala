package dotty.tools
package repl

import dotc.config.Properties.*
import dotc.config.CompilerCommand

object ReplCommand extends CompilerCommand:
  override def cmdName: String = "scala"
  override def versionMsg: String = s"Scala code runner $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scala -help  gives more information"
