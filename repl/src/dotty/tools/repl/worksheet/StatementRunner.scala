package dotty.tools.repl.worksheet

import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Phases.checkCapturesPhase
import dotty.tools.dotc.core.Phases.typerPhase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.str
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.repl.DependencyResolver
import dotty.tools.repl.Rendering
import dotty.tools.repl.Rendering.showUser
import dotty.tools.repl.ReplBytecodeInstrumentation
import dotty.tools.repl.ReplCompiler
import dotty.tools.repl.ScalaClassLoader.*
import dotty.tools.repl.State

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

private final case class StatementOutcome(
    rendered: Option[WorksheetStatement],
    state: State,
    failure: Option[WorksheetDiagnostic]
)

private final class StatementRunner(startup: ReplStartup, screenWidth: Int):
  @volatile private var cancelRequested = false

  def beginEvaluation(): Unit = cancelRequested = false

  def isCancelled: Boolean = cancelRequested

  private def rendering: Rendering = startup.driver.replRendering

  def addToClasspath(files: List[java.io.File], state: State): Unit =
    if files.nonEmpty then
      given Context = state.context
      val previous = rendering.classLoader()(using state.context)
      rendering.myClassLoader = DependencyResolver.addToCompilerClasspath(
        files,
        previous,
        state.context.settings.outputDir.value(using state.context)
      )

  def beginRun(state: State): Unit =
    ReplBytecodeInstrumentation.setStopFlag(
      rendering.classLoader()(using state.context),
      cancelRequested
    )

  def runOne(compiled: CompiledStatement, state: State): StatementOutcome =
    val loader = rendering.classLoader()(using state.context)
    loader.asContext:
      val (outcome, output) = StatementRunner.capturing:
        try Right(binders(compiled.objectIndex)(using state.context))
        catch
          case exception: ExceptionInInitializerError => Left(exception)
          case NonFatal(exception) => Left(exception)
      outcome match
        case Right(values) =>
          StatementOutcome(
            WorksheetRendering.render(compiled.input.position, values, output, screenWidth),
            state,
            None
          )
        case Left(exception) =>
          val cause = Rendering.rootCause(exception)
          val message =
            if cause.isInstanceOf[ThreadDeath] then WorksheetDiagnostic.cancelled
            else
              s"${cause.getClass.getName}: ${Option(cause.getMessage).getOrElse("")}"
                .stripSuffix(": ")
          StatementOutcome(
            None,
            state.invalidateCurrentObject,
            Some(
              WorksheetDiagnostic(
                compiled.input.position,
                message,
                WorksheetDiagnosticSeverity.Error
              )
            )
          )

  private def binders(objectIndex: Int)(using Context): List[RenderedBinder] =
    val renderPhase =
      if Feature.ccEnabledSomewhere && checkCapturesPhase.exists then checkCapturesPhase
      else typerPhase.next

    atPhase(renderPhase) {
      val path = nme.EMPTY_PACKAGE ++ "." ++ ReplCompiler.objectNames(objectIndex)
      val module = requiredModule(path)
      val rendered = module.info.fields
        .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
        .filter(_.symbol.name.is(SimpleNameKind))
        .toList
        .flatMap(binder)
      if rendered.isEmpty then
        Class.forName(module.moduleClass.fullName.encode.toString, true, rendering.classLoader())
      rendered
    }

  private def binder(denotation: Denotation)(using Context): Option[RenderedBinder] =
    val symbol = denotation.symbol
    val name = symbol.name.show.stripSuffix(str.REPL_ASSIGN_SUFFIX)
    val tpe = symbol.info.widen.show
    if symbol.is(Lazy) then Some(RenderedBinder.Declaration(symbol.showUser))
    else
      rendering
        .valueOf(symbol, s"$name: $tpe = ".length)
        .map(value => RenderedBinder.Value(name, tpe, value.plainText))

  def cancel(): Unit =
    cancelRequested = true
    Option(rendering.myClassLoader).foreach(ReplBytecodeInstrumentation.setStopFlag(_, true))

private object StatementRunner:
  private def capturing[A](body: => A): (A, String) =
    val buffer = new ByteArrayOutputStream
    val stream = new PrintStream(buffer, true, StandardCharsets.UTF_8)
    val result = Console.withOut(stream)(Console.withErr(stream)(body))
    stream.flush()
    (result, buffer.toString(StandardCharsets.UTF_8))
