package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**A basic implementation of VirtualFile that is only used when
 * the real virtual file can not be found.
 *
 * This has a very basic implementation of contentHash that is almost certainly colliding more than the implementation
 * in Zinc. It does not matter anyway as Zinc will recompile the associated source file, because it did not recieve the
 * same virtual file back.
 */
public class FallbackVirtualFile extends xsbti.BasicVirtualFileRef implements xsbti.VirtualFile {

  protected final SourceFile sourceFile;

  public FallbackVirtualFile(SourceFile sourceFile) {
    super(sourceFile.path());
    this.sourceFile = sourceFile;
  }

  private static byte[] toBytes(char[] chars) {
    return new String(chars).getBytes(StandardCharsets.UTF_8);
  }

  public InputStream input() {
    return new java.io.ByteArrayInputStream(toBytes(sourceFile.content()));
  }

  public long contentHash() {
    int murmurHash3 = scala.util.hashing.MurmurHash3$.MODULE$.bytesHash(toBytes(sourceFile.content()));
    return (long) murmurHash3;
  }

}
