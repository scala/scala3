package dotty.tools.repl.worksheet.interfaces;

import java.nio.file.Path;
import java.util.List;

/** Evaluates Scala worksheet source code. */
public interface WorksheetEvaluator extends AutoCloseable {
  /** @return A new evaluator using `classpath` to compile and run worksheets */
  WorksheetEvaluator withClasspath(List<Path> classpath);

  /** @return A new evaluator using the supplied Scala compiler options */
  WorksheetEvaluator withScalacOptions(List<String> options);

  /** @return A new evaluator fitting summaries to an editor `screenWidth` wide */
  WorksheetEvaluator withScreenWidth(int screenWidth);

  /** Evaluate a worksheet incrementally.
   *
   *  `text` is the complete current source, but only an unchanged prefix's appended
   *  suffix is re-run, without repeating the prefix's side effects. Any other change
   *  discards the session and runs everything. A failed suffix does not run at all.
   *
   *  @param filename A display filename, and the identity of the session to reuse
   *  @param text The complete worksheet source
   *  @return The whole worksheet, however much of it was reused
   */
  EvaluatedWorksheet evaluate(String filename, String text);

  /** Release resources owned by this evaluator, discarding any session. */
  void shutdown();

  @Override
  default void close() {
    shutdown();
  }
}
