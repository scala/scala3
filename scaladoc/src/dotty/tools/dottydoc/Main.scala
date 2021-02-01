package dotty.tools
package dottydoc


import dotc.reporting.Reporter
import dotc.Driver
import dotc.core.Contexts.Context

/** Main object for SBT.
  *
  * See [[this.process]].
  */
object Main extends Driver:

  /** Actual entrypoint from SBT.
    *
    * Internal SBT code for `sbt doc` locates this precise method with
    * reflection, and passes to us both `args` and `rootCtx`. "Internal" here
    * means that it's painful to modify this code with a plugin.
    *
    * `args` contains arguments both for us and for the compiler (see code on
    * how they're split).
    */
  override def process(args: Array[String], rootCtx: Context): Reporter =
    dotty.tools.scaladoc.Scaladoc.run(args, rootCtx)
