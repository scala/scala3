package dotty.tools

import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings

object TypeStealer {
  def main(args: Array[String]): Unit = {
    def repl = new ILoop {}

    val settings = new Settings
    settings.Yreplsync.value = true

    //use when launching normally outside SBT
    settings.usejavacp.value = true

    //an alternative to 'usejavacp' setting, when launching from within SBT
    //settings.embeddedDefaults[Repl.type]

    repl.process(settings)
  }
}
