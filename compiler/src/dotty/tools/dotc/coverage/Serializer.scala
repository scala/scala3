package dotty.tools.dotc
package coverage

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Path, Paths, Files}
import java.io.Writer
import scala.collection.mutable.StringBuilder
import scala.io.Source

/**
 * Serializes scoverage data.
 * @see https://github.com/scoverage/scalac-scoverage-plugin/blob/main/serializer/src/main/scala/scoverage/serialize/Serializer.scala
 */
object Serializer:

  private val CoverageFileName = "scoverage.coverage"
  private val CoverageDataFormatVersion = "3.0"

  def coverageFilePath(dataDir: String): Path =
    Paths.get(dataDir, CoverageFileName).toAbsolutePath

  /** Write out coverage data to the given data directory, using the default coverage filename */
  def serialize(coverage: Coverage, dataDir: String, sourceRoot: String): Unit =
    serialize(coverage, coverageFilePath(dataDir), Paths.get(sourceRoot).toAbsolutePath)

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
                      |${getRelativePath(stmt.location.sourcePath).escaped}
                      |${stmt.location.packageName.escaped}
                      |${stmt.location.className.escaped}
                      |${stmt.location.classType}
                      |${stmt.location.fullClassName.escaped}
                      |${stmt.location.methodName.escaped}
                      |${stmt.start}
                      |${stmt.end}
                      |${stmt.line}
                      |${stmt.symbolName.escaped}
                      |${stmt.treeName}
                      |${stmt.branch}
                      |0
                      |${stmt.ignored}
                      |${stmt.desc.escaped}
                      |\f
                      |""".stripMargin)

    writeHeader(writer)
    coverage.statements.toSeq
      .sortBy(_.id)
      .foreach(stmt => writeStatement(stmt, writer))

  def deserialize(file: Path, sourceRoot: String): Coverage =
    val source = Source.fromFile(file.toFile(), UTF_8.name())
    try deserialize(source.getLines(), Paths.get(sourceRoot).toAbsolutePath)
    finally source.close()

  def deserialize(lines: Iterator[String], sourceRoot: Path): Coverage =
    def toStatement(lines: Iterator[String]): Statement =
      val id: Int = lines.next().toInt
      val sourcePath = lines.next()
      val packageName = lines.next()
      val className = lines.next()
      val classType = lines.next()
      val fullClassName = lines.next()
      val method = lines.next()
      val loc = Location(
        packageName,
        className,
        fullClassName,
        classType,
        method,
        sourceRoot.resolve(sourcePath).normalize()
      )
      val start: Int = lines.next().toInt
      val end: Int = lines.next().toInt
      val lineNo: Int = lines.next().toInt
      val symbolName: String = lines.next()
      val treeName: String = lines.next()
      val branch: Boolean = lines.next().toBoolean
      val count: Int = lines.next().toInt
      val ignored: Boolean = lines.next().toBoolean
      val desc = lines.toList.mkString("\n")
      Statement(
        loc,
        id,
        start,
        end,
        lineNo,
        desc,
        symbolName,
        treeName,
        branch,
        ignored
      )

    val headerFirstLine = lines.next()
    require(
      headerFirstLine == s"# Coverage data, format version: $CoverageDataFormatVersion",
      "Wrong file format"
    )

    val linesWithoutHeader = lines.dropWhile(_.startsWith("#"))
    val coverage = Coverage()
    while !linesWithoutHeader.isEmpty do
      val oneStatementLines = linesWithoutHeader.takeWhile(_ != "\f")
      coverage.addStatement(toStatement(oneStatementLines))
    end while
    coverage

  /** Makes a String suitable for output in the coverage statement data as a single line.
   * Escaped characters: '\\' (backslash), '\n', '\r', '\f'
   */
  extension (str: String) def escaped: String =
    val builder = StringBuilder(str.length)
    var i = 0
    while
      i < str.length
    do
      str.charAt(i) match
        case '\\' =>
          builder ++= "\\\\"
        case '\n' =>
          builder ++= "\\n"
        case '\r' =>
          builder ++= "\\r"
        case '\f' =>
          builder ++= "\\f"
        case c =>
          builder += c
      i += 1
    end while
    builder.result()
