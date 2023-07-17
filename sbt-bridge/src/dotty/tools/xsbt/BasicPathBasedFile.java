package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;

public class BasicPathBasedFile extends PlaceholderVirtualFile implements xsbti.PathBasedFile {

  public BasicPathBasedFile(SourceFile sourceFile) {
    super(sourceFile);
  }

  public java.nio.file.Path toPath() {
    return sourceFile.file().jpath();
  }

}
