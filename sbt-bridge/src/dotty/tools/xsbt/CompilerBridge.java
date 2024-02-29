/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import xsbti.AnalysisCallback;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.VirtualFile;
import xsbti.compile.CompileProgress;
import xsbti.compile.CompilerInterface2;
import xsbti.compile.DependencyChanges;
import xsbti.compile.Output;

public final class CompilerBridge implements CompilerInterface2 {
  @Override
  public void run(VirtualFile[] sources, DependencyChanges changes, String[] options, Output output,
    AnalysisCallback callback, Reporter delegate, CompileProgress progress, Logger log) {
    CompilerBridgeDriver driver = new CompilerBridgeDriver(options, output);
    driver.run(sources, callback, log, delegate, progress);
  }
}
