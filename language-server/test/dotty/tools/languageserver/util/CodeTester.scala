package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.Code.SourceWithPositions
import dotty.tools.languageserver.util.actions._
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.{TestFile, TestServer}
import org.eclipse.lsp4j.SymbolInformation

class CodeTester(sources: List[SourceWithPositions], actions: List[Action]) {

  private val testServer = new TestServer(TestFile.testDir)

  private val files = sources.zipWithIndex.map { case (code, i) =>
    testServer.openCode(code.text, s"Source$i.scala")
  }
  private val positions: PositionContext = getPositions(files)

  def hover(range: CodeRange, expected: String): CodeTester =
    doAction(new CodeHover(range, expected))

  def definition(range: CodeRange, refOpt: Seq[CodeRange]): CodeTester =
    doAction(new CodeDefinition(range, refOpt))

  def highlight(range: CodeRange, highs: (CodeRange, String)*): CodeTester =
    doAction(new CodeDocumentHighlight(range, highs))

  def references(range: CodeRange, refs: List[CodeRange], withDecl: Boolean = false): CodeTester =
    doAction(new CodeReferences(range, refs, withDecl))

  def completion(marker: CodeMarker, completions: List[(String, String, String)]): CodeTester =
    doAction(new CodeCompletion(marker, completions))

  def rename(marker: CodeMarker, newName: String, expected: List[CodeRange]): CodeTester =
    doAction(new CodeRename(marker, newName, expected)) // TODO apply changes to the sources and positions

  def documentSymbol(marker: CodeMarker, symbols: SymInfo*): CodeTester =
    doAction(new CodeDocumentSymbol(marker, symbols))

  def symbol(query: String, symbols: SymInfo*): CodeTester =
    doAction(new CodeSymbol(query, symbols))

  private def doAction(action: Action): this.type = {
    try {
      action.execute()(testServer, positions)
    } catch {
      case ex: AssertionError =>
        val sourcesStr = sources.zip(files).map{ case (source, file) => "// " + file.file + "\n" + source.text}.mkString("\n")
        val msg =
          s"""
            |
            |$sourcesStr
            |
            |while executing action: ${action.show(positions)}
            |
          """.stripMargin
        val assertionError = new AssertionError(msg + ex.getMessage)
        assertionError.setStackTrace(ex.getStackTrace)
        throw assertionError
    }
    this
  }

  private def getPositions(files: List[TestFile]): PositionContext = {
    val posSeq = {
      for {
        (code, file) <- sources.zip(files)
        (position, line, char) <- code.positions
      } yield position -> (file, line, char)
    }
    val posMap = posSeq.toMap
    assert(posSeq.size == posMap.size,
      "Each CodeMarker instance can only appear once in the code: " + posSeq.map(x => (x._1, x._2._2, x._2._3)))
    new PositionContext(posMap)
  }
}
