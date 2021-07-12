/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.compile.*;

import dotty.tools.dotc.Main;

import java.lang.reflect.InvocationTargetException;
import java.io.File;

/**
 * The new compiler interface is [[dotty.tools.xsbt.CompilerBridge]] that extends the new `xsbti.CompilerInterface2`.
 * This interface is kept for compatibility with Mill and the sbt 1.3.x series.
 */
public final class CompilerInterface {
  public CachedCompiler newCompiler(String[] options, Output output, Logger initialLog, Reporter initialDelegate) throws java.lang.Exception {
    if(isClassLoaderValid()) {
      return new CachedCompilerImpl(options, output);
    } else {
      initialLog.warn(() ->
        "The compiler class loader is badly configured.\n" +
        "Consider using a more recent version of your build tool:\n" +
        " - sbt >= 1.5.0\n" +
        " - Mill >= 0.9.3-21-002361\n" +
        " - Bloop >= 1.4.6-23-20a501bc"
      );
      // To workaround the wrong class loader, we construct our own and run
      // the following code with it:
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
  }

  private boolean isClassLoaderValid() {
    // Check that the `xsbti.*` classes are loaded from the same class loader in the compiler and the bridge
    ClassLoader compilerClassLoader = Main.class.getClassLoader();
    Class<Logger> clazz = Logger.class;
    try {
      return compilerClassLoader.loadClass("xsbti.Logger") == clazz;
    } catch (ClassNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  public void run(File[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
      Reporter delegate, CompileProgress progress, CachedCompiler cached) {
    cached.run(sources, changes, callback, log, delegate, progress);
  }
}
