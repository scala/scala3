/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Severity;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ArrayList;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.reporting.Reporter;

public class DottydocRunner {
  private final String[] args0;
  private final Logger log;
  private final xsbti.Reporter delegate;

  public DottydocRunner(String[] args0, Logger log, xsbti.Reporter delegate) {
    super();
    this.args0 = args0;
    this.log = log;
    this.delegate = delegate;
  }

  public void run() {
    log.debug(() -> {
      StringBuilder msg =
        new StringBuilder("Calling Dottydoc with arguments  (ScaladocInterface):");
      for (String arg : args0) {
        msg.append("\n\t");
        msg.append(arg);
      }
      return msg.toString();
    });

    // When running with `-from-tasty`, remove the source files from arg list.
    String[] args;
    boolean fromTasty = false;
    for (String arg : args0) {
      if ("-from-tasty".equals(arg)) {
        fromTasty = true;
        break;
      }
    }
    if (fromTasty) {
      ArrayList<String> excluded = new ArrayList<>(args0.length);
      ArrayList<String> retained = new ArrayList<>(args0.length);
      for (String arg : args0) {
        if ((arg.endsWith(".scala") || arg.endsWith(".java")) && Files.exists(Paths.get(arg)))
          excluded.add(arg);
        else
          retained.add(arg);
      }
      log.debug(() -> {
        StringBuilder msg =
          new StringBuilder("Running `-from-tasty`, excluding source files:");
        for (String arg : excluded) {
          msg.append("\n\t");
          msg.append(arg);
        }
        return msg.toString();
      });
      args = retained.toArray(new String[retained.size()]);
    } else {
      args = args0;
    }

    Context ctx = new ContextBase().initialCtx().fresh()
      .setReporter(new DelegatingReporter(delegate));

    try {
      Class<?> dottydocMainClass = Class.forName("dotty.tools.dottydoc.Main");
      Method processMethod = dottydocMainClass.getMethod("process", args.getClass(), Context.class); // args.getClass() is String[]
      Reporter reporter = (Reporter) processMethod.invoke(null, args, ctx);
      if (reporter.hasErrors())
        throw new InterfaceCompileFailed(args, new xsbti.Problem[0]);
    } catch (ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      throw new RuntimeException(e);
    }
  }
}
