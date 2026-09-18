package dotty.tools.repl.worksheet.interfaces;

import java.nio.file.Path;
import java.util.List;

/** The result of evaluating one worksheet. */
public interface EvaluatedWorksheet {
  /** @return Compiler, directive and runtime diagnostics for the whole worksheet */
  List<Diagnostic> diagnostics();

  /** @return Successfully evaluated worksheet statements, in source order */
  List<EvaluatedWorksheetStatement> statements();

  /** @return The effective classpath: the configured one plus `dependencies()` */
  List<Path> classpath();

  /** @return Repositories the worksheet declared, as Maven URLs or Ivy patterns */
  List<String> repositories();

  /** @return Dependencies the worksheet declared, resolved and on `classpath()` */
  List<Dependency> dependencies();
}
