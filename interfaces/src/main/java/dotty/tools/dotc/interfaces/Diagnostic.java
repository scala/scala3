package dotty.tools.dotc.interfaces;

import java.util.Optional;

/** A diagnostic is a message emitted during the compilation process.
 *
 *  It can either be an error, a warning or an information.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface Diagnostic {
  public static final int ERROR = 2;
  public static final int WARNING = 1;
  public static final int INFO = 0;

  /** The message to report */
  String message();

  /** Level of the diagnostic, can be either ERROR, WARNING or INFO */
  int level();

  /** The position in a source file of the code that caused this diagnostic
   *  to be emitted. */
  Optional<SourcePosition> position();
}
