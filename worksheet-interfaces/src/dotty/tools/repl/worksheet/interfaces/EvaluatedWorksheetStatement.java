package dotty.tools.repl.worksheet.interfaces;

/** The output produced by one worksheet statement. */
public interface EvaluatedWorksheetStatement {
  /** @return The statement's range in the original worksheet source */
  RangePosition position();

  /** @return A short result suitable for an inline editor hint */
  String summary();

  /** @return The complete result suitable for a tooltip or an exported worksheet */
  String details();

  /** @return Whether `summary()` contains the complete result */
  boolean isSummaryComplete();
}
