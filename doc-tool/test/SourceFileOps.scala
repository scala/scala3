package dotty.tools
package dottydoc
package staticsite

import dotc.util.SourceFile
import java.io.{ BufferedWriter, OutputStreamWriter }
import io.VirtualFile
import scala.io.Codec

trait SourceFileOps {
  def stringToSource(path: String, sourceCode: String): SourceFile = {
    val virtualFile = new VirtualFile(path, path)
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8"))
    writer.write(sourceCode)
    writer.close()

    new SourceFile(virtualFile, Codec.UTF8)
  }
}
