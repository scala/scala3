package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;

/**A basic implementation of PathBasedFile that is only used when
 * the real virtual file can not be found.
 *
 * See FallbackVirtualFile for more details.
 */
public class FallbackPathBasedFile extends FallbackVirtualFile implements xsbti.PathBasedFile {

  public FallbackPathBasedFile(SourceFile sourceFile) {
    super(sourceFile);
  }

  public java.nio.file.Path toPath() {
    return sourceFile.file().jpath();
  }

}
