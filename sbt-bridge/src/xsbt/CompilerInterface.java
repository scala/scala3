/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.Severity;
import xsbti.compile.*;

import java.io.File;

import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.Main;
import dotty.tools.dotc.interfaces.*;

import java.lang.reflect.InvocationTargetException;
import java.net.URLClassLoader;

public final class CompilerInterface {
  public CachedCompiler newCompiler(String[] options, Output output, Logger initialLog, Reporter initialDelegate) {
    // The classloader that sbt uses to load the compiler bridge is broken
    // (see CompilerClassLoader#fixBridgeLoader for details). To workaround
    // this we construct our own ClassLoader and then run the following code
    // with it:
    //   new CachedCompilerImpl(options, output)

    try {
      ClassLoader bridgeLoader = this.getClass().getClassLoader();
      ClassLoader fixedLoader = CompilerClassLoader.fixBridgeLoader(bridgeLoader);
      Class<?> cciClass = fixedLoader.loadClass("xsbt.CachedCompilerImpl");
      return (CachedCompiler) cciClass.getConstructors()[0].newInstance(options, output);
    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | InvocationTargetException e) {
      throw new RuntimeException(e);
    }
  }

  public void run(File[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
      Reporter delegate, CompileProgress progress, CachedCompiler cached) {
    cached.run(sources, changes, callback, log, delegate, progress);
  }
}
