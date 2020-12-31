/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.compile.*;

import java.io.File;

/**
 * The new compiler interface is [[dotty.tools.xsbt.CompilerBridge]] that extends the new `xsbti.CompilerInterface2`.
 * This interface is kept for compatibility with Mill and the sbt 1.3.x series.
 */
public final class CompilerInterface {
  public CachedCompiler newCompiler(String[] options, Output output, Logger initialLog, Reporter initialDelegate) {
    return new CachedCompilerImpl(options, output);
  }

  public void run(File[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
      Reporter delegate, CompileProgress progress, CachedCompiler cached) {
    cached.run(sources, changes, callback, log, delegate, progress);
  }
}
