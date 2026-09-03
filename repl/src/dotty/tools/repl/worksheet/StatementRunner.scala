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
import dotty.tools.repl.Rendering
import dotty.tools.repl.ReplBytecodeInstrumentation
import dotty.tools.repl.ReplCompiler
import dotty.tools.repl.ScalaClassLoader.fromURLsParallelCapable
import dotty.tools.repl.ScalaClassLoader.*
import dotty.tools.repl.State

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

private final case class StatementOutcome(
    rendered: Option[WorksheetStatement],
    state: State,
    failure: Option[WorksheetDiagnostic],
    cancelled: Boolean = false
)

private final class StatementRunner(startup: ReplStartup, screenWidth: Int):
  @volatile private var runtime: Option[(URLClassLoader, Rendering)] = None

  private def rendering: Rendering = runtime match
    case Some((_, loaded)) => loaded
    case None =>
      val context = startup.initialState.context
      val parent = fromURLsParallelCapable(
        context.platform.classPath(using context).asURLs,
        getClass.getClassLoader
      )
      val loaded = new Rendering(Some(parent))
      runtime = Some((parent, loaded))
      loaded

  def beginRun(state: State): Unit =
    ReplBytecodeInstrumentation.setStopFlag(rendering.classLoader()(using state.context), false)

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
          val cancelled = cause.isInstanceOf[ThreadDeath]
          val message =
            if cancelled then "The worksheet evaluation was cancelled."
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
            ),
            cancelled
          )

  private def binders(objectIndex: Int)(using Context): List[RenderedBinder] =
    val renderPhase =
      if Feature.ccEnabledSomewhere && checkCapturesPhase.exists then checkCapturesPhase
      else typerPhase.next

    atPhase(renderPhase) {
      val path = nme.EMPTY_PACKAGE ++ "." ++ ReplCompiler.objectNames(objectIndex)
      requiredModule(path).info.fields
        .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
        .filter(_.symbol.name.is(SimpleNameKind))
        .toList
        .filterNot(_.symbol.is(Lazy))
        .flatMap(binder)
    }

  private def binder(denotation: Denotation)(using Context): Option[RenderedBinder] =
    val symbol = denotation.symbol
    val name = symbol.name.show.stripSuffix(str.REPL_ASSIGN_SUFFIX)
    val tpe = symbol.info.widen.show
    rendering
      .valueOf(symbol, s"$name: $tpe = ".length)
      .map(value => RenderedBinder(name, tpe, value.plainText))

  def cancel(): Unit =
    runtime.foreach: (_, loaded) =>
      ReplBytecodeInstrumentation.setStopFlag(
        loaded.classLoader()(using startup.initialState.context),
        true
      )

  def close(): Unit =
    runtime.foreach((parent, _) => parent.close())
    runtime = None

private object StatementRunner:
  private def capturing[A](body: => A): (A, String) =
    val buffer = new ByteArrayOutputStream
    val stream = new PrintStream(buffer, true, StandardCharsets.UTF_8)
    val result = Console.withOut(stream)(Console.withErr(stream)(body))
    stream.flush()
    (result, buffer.toString(StandardCharsets.UTF_8))
