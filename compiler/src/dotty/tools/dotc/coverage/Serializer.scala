package dotty.tools.dotc
package coverage

import java.nio.file.{Path, Paths, Files}
import java.io.Writer
import scala.language.unsafeNulls

/**
 * Serializes scoverage data.
 * @see https://github.com/scoverage/scalac-scoverage-plugin/blob/main/scalac-scoverage-plugin/src/main/scala/scoverage/Serializer.scala
 */
object Serializer:

  private val CoverageFileName = "scoverage.coverage"
  private val CoverageDataFormatVersion = "3.0"

  /** Write out coverage data to the given data directory, using the default coverage filename */
  def serialize(coverage: Coverage, dataDir: String, sourceRoot: String): Unit =
    serialize(coverage, Paths.get(dataDir, CoverageFileName).toAbsolutePath, Paths.get(sourceRoot).toAbsolutePath)

  /** Write out coverage data to a file. */
  def serialize(coverage: Coverage, file: Path, sourceRoot: Path): Unit =
    val writer = Files.newBufferedWriter(file)
    try
      serialize(coverage, writer, sourceRoot)
    finally
      writer.close()

  /** Write out coverage data (info about each statement that can be covered) to a writer.
   */
  def serialize(coverage: Coverage, writer: Writer, sourceRoot: Path): Unit =

    def getRelativePath(filePath: Path): String =
      // We need to normalize the path here because the relativizing paths containing '.' or '..' differs between Java versions
      // https://bugs.openjdk.java.net/browse/JDK-8066943
      val relPath = sourceRoot.normalize.relativize(filePath)
      relPath.toString

    def writeHeader(writer: Writer): Unit =
      writer.write(s"""# Coverage data, format version: $CoverageDataFormatVersion
                      |# Statement data:
                      |# - id
                      |# - source path
                      |# - package name
                      |# - class name
                      |# - class type (Class, Object or Trait)
                      |# - full class name
                      |# - method name
                      |# - start offset
                      |# - end offset
                      |# - line number
                      |# - symbol name
                      |# - tree name
                      |# - is branch
                      |# - invocations count
                      |# - is ignored
                      |# - description (can be multi-line)
                      |# '\f' sign
                      |# ------------------------------------------
                      |""".stripMargin)

    def writeStatement(stmt: Statement, writer: Writer): Unit =
      // Note: we write 0 for the count because we have not measured the actual coverage at this point
      writer.write(s"""${stmt.id}
                      |${getRelativePath(stmt.location.sourcePath)}
                      |${stmt.location.packageName}
                      |${stmt.location.className}
                      |${stmt.location.classType}
                      |${stmt.location.fullClassName}
                      |${stmt.location.method}
                      |${stmt.start}
                      |${stmt.end}
                      |${stmt.line}
                      |${stmt.symbolName}
                      |${stmt.treeName}
                      |${stmt.branch}
                      |0
                      |${stmt.ignored}
                      |${stmt.desc}
                      |\f
                      |""".stripMargin)

    writeHeader(writer)
    coverage.statements.toSeq
      .sortBy(_.id)
      .foreach(stmt => writeStatement(stmt, writer))
