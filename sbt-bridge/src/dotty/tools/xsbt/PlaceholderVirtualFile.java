package dotty.tools.xsbt;

import dotty.tools.dotc.interfaces.SourceFile;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public final class PlaceholderVirtualFile extends xsbti.BasicVirtualFileRef implements xsbti.VirtualFile {

  private final SourceFile sourceFile;

  public PlaceholderVirtualFile(SourceFile sourceFile) {
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
    return scala.util.hashing.package$.MODULE$.byteswap64((long) murmurHash3);
  }

}
