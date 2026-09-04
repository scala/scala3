package dotty.tools.repl.worksheet

import dotty.tools.repl.worksheet.interfaces.Dependency as ApiDependency
import dotty.tools.repl.worksheet.interfaces.Diagnostic as ApiDiagnostic
import dotty.tools.repl.worksheet.interfaces.DiagnosticSeverity as ApiDiagnosticSeverity
import dotty.tools.repl.worksheet.interfaces.EvaluatedWorksheet as ApiEvaluation
import dotty.tools.repl.worksheet.interfaces.WorksheetEvaluator as ApiEvaluator
import dotty.tools.repl.worksheet.interfaces.RangePosition as ApiPosition
import dotty.tools.repl.worksheet.interfaces.EvaluatedWorksheetStatement as ApiStatement

import java.io.File
import java.nio.file.Path
import java.util.List as JavaList
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.util.Try

final class WorksheetDriver private (configuration: WorksheetConfiguration) extends ApiEvaluator:
  private var session: Option[WorksheetSession] = None

  def this() = this(WorksheetConfiguration.default)

  private def compiler: WorksheetSession = synchronized:
    session.getOrElse:
      val started = new WorksheetSession(
        configuration.compilerSettings,
        configuration.screenWidth
      )
      session = Some(started)
      started

  private[worksheet] def isSessionStarted: Boolean = synchronized(session.isDefined)

  override def withClasspath(classpath: JavaList[Path]): WorksheetDriver =
    new WorksheetDriver(configuration.copy(classpath = classpath.asScala.toList))

  override def withScalacOptions(options: JavaList[String]): WorksheetDriver =
    new WorksheetDriver(configuration.copy(scalacOptions = options.asScala.toList))

  override def withScreenWidth(screenWidth: Int): WorksheetDriver =
    require(screenWidth > 0, "screenWidth must be greater than zero")
    new WorksheetDriver(configuration.copy(screenWidth = screenWidth))

  override def evaluate(filename: String, text: String): ApiEvaluation =
    val result = compiler.evaluate(filename, text)
    ApiEvaluationImpl.from(result, configuration.classpath)

  override def cancel(): Unit =
    synchronized(session).foreach(_.cancel())

  override def shutdown(): Unit = synchronized:
    session.foreach(_.shutdown())
    session = None

private final case class WorksheetConfiguration(
    classpath: List[Path],
    scalacOptions: List[String],
    screenWidth: Int
):
  def compilerSettings: Array[String] =
    val compilationClasspath =
      (classpath ::: WorksheetConfiguration.supportClasspath).distinct
    val classpathSettings =
      if compilationClasspath.isEmpty then "-usejavacp" :: Nil
      else "-classpath" :: compilationClasspath.mkString(File.pathSeparator) :: Nil
    (scalacOptions ::: classpathSettings ::: List("-color:never")).toArray

private object WorksheetConfiguration:
  val default: WorksheetConfiguration = WorksheetConfiguration(Nil, Nil, 120)

  val supportClasspath: List[Path] =
    List(
      classOf[WorksheetDriver],
      classOf[dotty.tools.dotc.Compiler],
      classOf[dotty.tools.dotc.interfaces.Diagnostic],
      classOf[scala.deriving.Mirror],
      classOf[scala.Option[?]]
    ).flatMap(classpathEntry).distinct

  private def classpathEntry(clazz: Class[?]): Option[Path] =
    Option(clazz.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .flatMap(location => Try(Path.of(location.toURI)).toOption)

private final class ApiEvaluationImpl(
    evaluatedDiagnostics: JavaList[ApiDiagnostic],
    evaluatedStatements: JavaList[ApiStatement],
    effectiveClasspath: JavaList[Path],
    resolvedRepositories: JavaList[String],
    resolvedDependencies: JavaList[ApiDependency]
) extends ApiEvaluation:
  override def diagnostics(): JavaList[ApiDiagnostic] = evaluatedDiagnostics
  override def statements(): JavaList[ApiStatement] = evaluatedStatements
  override def classpath(): JavaList[Path] = effectiveClasspath
  override def repositories(): JavaList[String] = resolvedRepositories
  override def dependencies(): JavaList[ApiDependency] = resolvedDependencies

private object ApiEvaluationImpl:
  def from(result: WorksheetResult, configuredClasspath: List[Path]): ApiEvaluation =
    new ApiEvaluationImpl(
      immutableJavaList(result.diagnostics.map(ApiDiagnosticImpl(_))),
      immutableJavaList(result.statements.map(ApiStatementImpl(_))),
      immutableJavaList(configuredClasspath),
      JavaList.of(),
      JavaList.of()
    )

private final class ApiStatementImpl(statement: WorksheetStatement) extends ApiStatement:
  private val evaluatedPosition = ApiPositionImpl(statement.position)

  override def position(): ApiPosition = evaluatedPosition
  override def summary(): String = statement.summary
  override def details(): String = statement.details
  override def isSummaryComplete(): Boolean = statement.isSummaryComplete

private final class ApiDiagnosticImpl(diagnostic: WorksheetDiagnostic) extends ApiDiagnostic:
  private val evaluatedPosition = ApiPositionImpl(diagnostic.position)

  override def position(): ApiPosition = evaluatedPosition
  override def message(): String = diagnostic.message
  override def severity(): ApiDiagnosticSeverity =
    diagnostic.severity match
      case WorksheetDiagnosticSeverity.Info => ApiDiagnosticSeverity.Info
      case WorksheetDiagnosticSeverity.Warning => ApiDiagnosticSeverity.Warning
      case WorksheetDiagnosticSeverity.Error => ApiDiagnosticSeverity.Error

private final class ApiPositionImpl(position: WorksheetPosition) extends ApiPosition:
  override def startLine(): Int = position.startLine
  override def startColumn(): Int = position.startColumn
  override def endLine(): Int = position.endLine
  override def endColumn(): Int = position.endColumn

private def immutableJavaList[A](values: List[A]): JavaList[A] =
  JavaList.copyOf(values.asJava)
