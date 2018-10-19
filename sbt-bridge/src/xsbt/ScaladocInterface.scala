/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{ Logger, Severity }
import java.net.URL
import java.util.Optional

import dotty.tools.dotc.core.Contexts.{ Context, ContextBase }
import dotty.tools.dotc.reporting.Reporter

class ScaladocInterface {
  def run(args: Array[String], log: Logger, delegate: xsbti.Reporter) = {
    new DottydocRunner(args, log, delegate).run()
  }
}

class DottydocRunner(args: Array[String], log: Logger, delegate: xsbti.Reporter) {
  def run(): Unit = {
    log.debug(() => args.mkString("Calling Dottydoc with arguments  (ScaladocInterface):\n\t", "\n\t", ""))

    val ctx = (new ContextBase).initialCtx.fresh
      .setReporter(new DelegatingReporter(delegate))

    val dottydocMainClass = Class.forName("dotty.tools.dottydoc.Main")
    val processMethod = dottydocMainClass.getMethod("process", classOf[Array[String]], classOf[Context])
    val reporter = processMethod.invoke(null, args, ctx).asInstanceOf[Reporter]
    if (reporter.hasErrors) {
      throw new InterfaceCompileFailed(args, Array())
    }
  }
}
