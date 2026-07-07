package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;

/**A basic implementation of PathBasedFile that is only used when
 * the real virtual file can not be found.
 *
 * See FallbackVirtualFile for more details.
 */
public class FallbackPathBasedFile extends FallbackVirtualFile implements xsbti.PathBasedFile {

  private final java.nio.file.Path path;

  public FallbackPathBasedFile(SourceFile sourceFile, java.nio.file.Path path) {
    super(sourceFile);
    this.path = path;
  }

  public java.nio.file.Path toPath() {
    return path;
  }

}
