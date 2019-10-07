package dotty.tools.dotc.semanticdb
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("missing <out>")
    }
    val docs = TextDocuments(
      List(
        TextDocument(
          Schema.SEMANTICDB3,
          Language.SCALA,
          uri = "Main.scala",
          "md5",
          List(
            SymbolOccurrence(
              "scala/Predef.String#",
              Some(Range(0, 0, 1, 1)),
              SymbolOccurrence.Role.REFERENCE
            )
          )
        )
      )
    )
    println(docs.serializedSize)
    val path = Paths.get(args(0)).toAbsolutePath()
    val out = Files.newOutputStream(
      path,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    val stream = SemanticdbOutputStream.newInstance(out)
    try {
      docs.writeTo(stream)
    } finally {
      stream.flush()
      out.close()
    }
    println(path)
  }
}
