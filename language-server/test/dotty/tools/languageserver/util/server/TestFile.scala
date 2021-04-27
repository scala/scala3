package dotty.tools.languageserver.util.server

import java.nio.file.{Path, Paths}

import org.eclipse.lsp4j.TextDocumentIdentifier

class TestFile(val file: String) extends AnyVal {
  def uri: String = TestFile.sourceDir.resolve(file).toUri.toString
}

object TestFile {
  lazy val testDir: Path = Paths.get("../out/ide-tests").toAbsolutePath.normalize
  lazy val sourceDir: Path = testDir.resolve("src")
}
