package dotty.tools.dotc

import config.Properties.*
import config.CompilerCommand

object ScalacCommand extends CompilerCommand:
  override def cmdName: String = "scalac"
  override def versionMsg: String = s"Scala compiler $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scalac -help  gives more information"
