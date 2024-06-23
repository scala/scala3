package dotty.tools.xsbt;

import dotty.tools.dotc.sbt.interfaces.ProgressCallback;
import dotty.tools.dotc.CompilationUnit;

import xsbti.compile.CompileProgress;

public final class ProgressCallbackImpl implements ProgressCallback {
  private boolean _cancelled = false; // TODO: atomic boolean?
  private final CompileProgress _progress;

  public ProgressCallbackImpl(CompileProgress progress) {
    _progress = progress;
  }

  @Override
  public void cancel() {
    _cancelled = true;
  }

  @Override
  public boolean isCancelled() {
    return _cancelled;
  }

  @Override
  public void informUnitStarting(String phase, CompilationUnit unit) {
    _progress.startUnit(phase, unit.source().file().path());
  }

  @Override
  public boolean progress(int current, int total, String currPhase, String nextPhase) {
    return _progress.advance(current, total, currPhase, nextPhase);
  }
}
