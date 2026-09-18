package dotty.tools.repl.worksheet.interfaces;

/** A zero-based range in the original worksheet source. */
public interface RangePosition {
  int startLine();
  int startColumn();
  int endLine();
  int endColumn();
}
