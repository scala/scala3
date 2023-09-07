package dotty.tools.scaladoc

import dotty.tools.dotc.core.Contexts.ContextBase

/** Main class for the doctool when used from cli. */
class Main:
  // This is an entry point for Mill or other tools in the future since it provides easy access to reporter
  def run(args: Array[String]) = Scaladoc.run(args, (new ContextBase).initialCtx)

object Main:
  def main(args: Array[String]): Unit =
  try
    // We should create our own context here...
    val reporter = Main().run(args)
    // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
    sys.exit(if reporter.hasErrors then 1 else 0)
  catch
    case a: Exception =>
      a.printStackTrace()
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(1)
