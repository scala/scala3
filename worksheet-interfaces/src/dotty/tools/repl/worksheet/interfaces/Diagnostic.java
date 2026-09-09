package dotty.tools.repl.worksheet.interfaces;

/** A diagnostic produced while compiling or running a worksheet. */
public interface Diagnostic {
  /** @return The diagnostic's range in the original worksheet source */
  RangePosition position();

  /** @return The diagnostic message */
  String message();

  /** @return The diagnostic severity */
  DiagnosticSeverity severity();
}
