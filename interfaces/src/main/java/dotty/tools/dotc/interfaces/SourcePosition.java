package dotty.tools.dotc.interfaces;

/** A position in a source file.
 *
 *  A position is a range between a start offset and an end offset, as well as a
 *  point inside this range.
 *
 *  As a convenience, we also provide methods that return the line and the column
 *  corresponding to each offset.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface SourcePosition {
  /** Content of the line which contains the point */
  String lineContent();

  /** Offset to the point */
  int point();
  /** Line number of the point, starting at 0 */
  int line();
  /** Column number of the point, starting at 0 */
  int column();

  /** Offset to the range start */
  int start();
  /** Line number of the range start, starting at 0 */
  int startLine();
  /** Column number of the range start, starting at 0 */
  int startColumn();

  /** Offset to the range end */
  int end();
  /** Line number of the range end, starting at 0 */
  int endLine();
  /** Column number of the range end, starting at 0 */
  int endColumn();

  /** The source file corresponding to this position.
   *  The values returned by `point()`, `start()` and `end()`
   *  are indices in the array returned by `source().content()`.
   */
  SourceFile source();
}
