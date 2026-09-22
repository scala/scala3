package dotty.tools.dotc.interfaces;

/** A source file.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface SourceFile extends AbstractFile {
  /** @return The content of this file as seen by the compiler.
   *  @deprecated This method is less efficient than `textContent` given the compiler implementation. */
  @Deprecated(since="3.10")
  char[] content();

  /** @return The text contents of this file as seen by the compiler. */
  default String textContent() {
    return new String(content());
  }
}
