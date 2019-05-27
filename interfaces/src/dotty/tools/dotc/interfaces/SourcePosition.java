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
  /** @return Content of the line which contains the point */
  String lineContent();

  /** @return Offset to the point */
  int point();
  /** @return Line number of the point, starting at 0. -1 if the line cannot be computed */
  int line();
  /** @return Column number of the point, starting at 0. -1 if the column cannot be computed */
  int column();

  /** @return Offset to the range start */
  int start();
  /** @return Line number of the range start, starting at 0. -1 if the line cannot be computed */
  int startLine();
  /** @return Column number of the range start, starting at 0. -1 if the column cannot be computed */
  int startColumn();

  /** @return Offset to the range end */
  int end();
  /** @return Line number of the range end, starting at 0. -1 if the line cannot be computed */
  int endLine();
  /** @return Column number of the range end, starting at 0. -1 if the column cannot be computed */
  int endColumn();

  /** The source file corresponding to this position.
   *  The values returned by `point()`, `start()` and `end()`
   *  are indices in the array returned by `source().content()`.
   *  @return source file for this position
   */
  SourceFile source();
}
