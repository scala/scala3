package test
import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings

/**
 * Dotty requires a mangled bootclasspath to start. It means that `console` mode of sbt doesn't work for us.
 * At least I(Dmitry) wasn't able to make sbt fork in console
 */
object DottyRepl {
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
