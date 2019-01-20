package dotty.tools.dottydoc

import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{Path, PlainFile}

object SourceUtil {

  /** Create a temporary `.scala` source file with the given content */
  def makeTemp(content: String): SourceFile = {
    val tempFile = java.io.File.createTempFile("dottydoc-test-", ".scala")
    tempFile.deleteOnExit()
    val file = new PlainFile(Path(tempFile.toPath))
    val out = file.output
    out.write(content.getBytes)
    new SourceFile(file, scala.io.Codec.UTF8)
  }

}
