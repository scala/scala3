package dotty.tools.dotc.interfaces;

import java.io.File;
import java.util.Optional;

/** An abstract file may either be a file on disk or a virtual file.
 *
 *  Do not rely on the identity of instances of this class.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
public interface AbstractFile {
  /** @return The name of this file, note that two files may have the same name. */
  String name();

  /** @return The path of this file, this might be a virtual path of an unspecified format. */
  String path();

  /** @return If this is a real file on disk, a `java.io.File` that corresponds to this file.
   *  Otherwise, an empty `Optional`.
   */
  Optional<File> jfile();
}
