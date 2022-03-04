package dotty.tools.dotc
package coverage

import java.io._

import scala.io.Source

/**
 * Serializes scoverage data.
 * @see https://github.com/scoverage/scalac-scoverage-plugin/blob/main/scalac-scoverage-plugin/src/main/scala/scoverage/Serializer.scala
 */
object Serializer:

  private val CoverageFileName = "scoverage.coverage"
  private val CoverageDataFormatVersion = "3.0"

  /** Write out coverage data to the given data directory, using the default coverage filename */
  def serialize(coverage: Coverage, dataDir: String, sourceRoot: String): Unit =
    serialize(coverage, coverageFile(dataDir), new File(sourceRoot))

  /** Write out coverage data to given file. */
  def serialize(coverage: Coverage, file: File, sourceRoot: File): Unit =
    val writer = BufferedWriter(FileWriter(file))
    serialize(coverage, writer, sourceRoot)
    writer.close()

  def serialize(coverage: Coverage, writer: Writer, sourceRoot: File): Unit =

    def getRelativePath(filePath: String): String =
      val base = sourceRoot.getCanonicalFile().toPath()
      val relPath = base.relativize(File(filePath).getCanonicalFile().toPath())
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
                      |${stmt.count}
                      |${stmt.ignored}
                      |${stmt.desc}
                      |\f
                      |""".stripMargin)

    writeHeader(writer)
    coverage.statements.toSeq
      .sortBy(_.id)
      .foreach(stmt => writeStatement(stmt, writer))

  def coverageFile(dataDir: File): File = coverageFile(dataDir.getAbsolutePath)
  def coverageFile(dataDir: String): File = File(dataDir, CoverageFileName)
