/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import xsbti.{ Logger, Severity }
import java.net.URL
import java.util.Optional
import java.nio.file.{Files, Paths}

import dotty.tools.dotc.core.Contexts.{ Context, ContextBase }
import dotty.tools.dotc.reporting.Reporter

class ScaladocInterface {
  def run(args: Array[String], log: Logger, delegate: xsbti.Reporter) = {
    new DottydocRunner(args, log, delegate).run()
  }
}

class DottydocRunner(args0: Array[String], log: Logger, delegate: xsbti.Reporter) {
  def run(): Unit = {
    log.debug(() => args0.mkString("Calling Dottydoc with arguments  (ScaladocInterface):\n\t", "\n\t", ""))

    val args = {
      // When running with `-from-tasty`, remove the source files from arg list.
      if (args0.contains("-from-tasty")) {
        val (excluded, retained) =
          args0.partition { arg =>
            (arg.endsWith(".scala") || arg.endsWith(".java")) && Files.exists(Paths.get(arg))
          }
        log.debug(() => excluded.mkString("Running `-from-tasty`, excluding source files:\n\t", "\n\t", ""))
        retained
      } else args0
    }

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
