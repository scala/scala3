package scala.meta.internal.pc

import java.net.URI
import java.nio.file.Paths

import dotty.tools.dotc.util.SourceFile

// note(@tgodzik) the plan is to be able to move the methods here back to Dotty compiler
// so that we can provide easier compatibility with multiple Scala 3 versions
object CompilerInterfaces:

  def toSource(uri: URI, sourceCode: String): SourceFile =
    val path = Paths.get(uri).toString
    SourceFile.virtual(path, sourceCode)

end CompilerInterfaces
