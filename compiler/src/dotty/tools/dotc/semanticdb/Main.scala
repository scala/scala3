package dotty.tools.dotc.semanticdb

import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import java.nio.file.Path
import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

object Main {
  def write(
      source: Path,
      contents: String,
      sourceroot: Path,
      targetroot: Path,
      occurrences: Seq[SymbolOccurrence]
  ): Unit = {
    val relpath = sourceroot.relativize(source)
    val reluri = relpath.iterator().asScala.mkString("/")
    val outpath = targetroot
      .resolve("META-INF")
      .resolve("semanticdb")
      .resolve(relpath)
      .resolveSibling(source.getFileName().toString() + ".semanticdb")
    Files.createDirectories(outpath.getParent())
    val doc: TextDocument = TextDocument(
      schema = Schema.SEMANTICDB4,
      language = Language.SCALA,
      uri = reluri,
      text = "",
      md5 = MD5.compute(contents),
      occurrences = occurrences
    )
    val docs = TextDocuments(List(doc))
    val out = Files.newOutputStream(outpath)
    try {
      val stream = SemanticdbOutputStream.newInstance(out)
      docs.writeTo(stream)
      stream.flush()
    } finally {
      out.close()
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("missing <sourceroot>")
      sys.exit(1)
    }
    val occurrences = List(
      SymbolOccurrence(
        "_empty_/Main.",
        Some(Range(0, 8, 0, 12)),
        SymbolOccurrence.Role.REFERENCE
      )
    )
    val sourceroot = Paths.get(args(0))
    val targetroot = sourceroot.resolve("out")
    write(
      sourceroot.resolve("Main.scala"),
      "object Main",
      sourceroot,
      targetroot,
      occurrences
    )
  }
}
