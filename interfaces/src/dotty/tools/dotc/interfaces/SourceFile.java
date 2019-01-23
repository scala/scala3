/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.interfaces;

/** A source file.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface SourceFile extends AbstractFile {
  /** @return The content of this file as seen by the compiler. */
  char[] content();
}
