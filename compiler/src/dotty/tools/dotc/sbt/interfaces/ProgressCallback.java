package dotty.tools.dotc.sbt.interfaces;

import dotty.tools.dotc.CompilationUnit;

public interface ProgressCallback {
  /** Record that the cancellation signal has been recieved during the Zinc run. */
  default void cancel() {}

  /** Report on if there was a cancellation signal for the current Zinc run. */
  default boolean isCancelled() { return false; }

  /** Record that a unit has started compiling in the given phase. */
  default void informUnitStarting(String phase, CompilationUnit unit) {}

  /** Record the current compilation progress.
   *  @param current `completedPhaseCount * totalUnits + completedUnitsInCurrPhase + completedLate`
   *  @param total `totalPhases * totalUnits + totalLate`
   *  @return true if the compilation should continue (if false, then subsequent calls to `isCancelled()` will return true)
   */
  default boolean progress(int current, int total, String currPhase, String nextPhase) { return true; }
}
