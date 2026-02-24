package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import dotc.ast.Trees.*
import dotc.ast.{tpd, untpd}
import dotc.classpath.ClassPathFactory
import dotc.config.CommandLineParser.tokenize
import dotc.config.Properties.{javaVersion, javaVmName, simpleVersionString}
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Phases.{unfusedPhases, typerPhase}
import dotc.core.Denotations.Denotation
import dotc.core.Flags.*
import dotc.core.Mode
import dotc.core.NameKinds.SimpleNameKind
import dotc.core.NameKinds.DefaultGetterName
import dotc.core.NameOps.*
import dotc.core.Names.Name
import dotc.core.StdNames.*
import dotc.core.Symbols.{Symbol, defn}
import dotc.core.SymbolLoaders
import dotc.interfaces
import dotc.interactive.Completion
import dotc.printing.SyntaxHighlighting
import dotc.reporting.{ConsoleReporter, StoreReporter}
import dotc.reporting.Diagnostic
import dotc.util.Spans.Span
import dotc.util.{SourceFile, SourcePosition}
import dotc.{CompilationUnit, Driver}
import dotc.config.CompilerCommand
import dotty.tools.io.{AbstractFileClassLoader => _, *}
import dotty.tools.repl.ScalaClassLoader.*

import org.jline.reader.*

import Rendering.showUser

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.tools.asm.ClassReader
import scala.util.control.NonFatal
import scala.util.Using

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex    the index of next value binding for free expressions
 *  @param imports     a map from object index to the list of user defined imports
 *  @param invalidObjectIndexes the set of object indexes that failed to initialize
 *  @param quiet       whether we print evaluation results
 *  @param context     the latest compiler context
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 imports: Map[Int, List[tpd.Import]],
                 invalidObjectIndexes: Set[Int],
                 quiet: Boolean,
                 context: Context):
  def validObjectIndexes = (1 to objectIndex).filterNot(invalidObjectIndexes.contains(_))

/** Main REPL instance, orchestrating input, compilation and presentation */
class ReplDriver(settings: Array[String],
                 out: PrintStream = Console.out,
                 classLoader: Option[ClassLoader] = None,
                 extraPredef: String = "") extends Driver:

  /** Overridden to `false` in order to not have to give sources on the
   *  commandline
   */
  override def sourcesRequired: Boolean = false

  /** Create a fresh and initialized context with IDE mode enabled */
  private def initialCtx(settings: List[String]) = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.XcookComments, true)
    rootCtx.setSetting(rootCtx.settings.XreadComments, true)
    setupRootCtx(this.settings ++ settings, rootCtx)
  }

  private val incompatibleOptions: Seq[String] = Seq(
    initCtx.settings.YbestEffort.name,
    initCtx.settings.YwithBestEffortTasty.name
  )

  private def setupRootCtx(settings: Array[String], rootCtx: Context) = {
    val incompatible = settings.intersect(incompatibleOptions)
    val filteredSettings =
      if !incompatible.isEmpty then
        inContext(rootCtx) {
          out.println(i"Options incompatible with repl will be ignored: ${incompatible.mkString(", ")}")
        }
        settings.filter(!incompatible.contains(_))
      else settings
    setup(filteredSettings, rootCtx) match
      case Some((files, ictx)) => inContext(ictx) {
        shouldStart = true
        if files.nonEmpty then out.println(i"Ignoring spurious arguments: $files%, %")
        ictx.base.initialize()
        ictx
      }
      case None =>
        shouldStart = false
        rootCtx
  }

  /** the initial, empty state of the REPL session */
  final def initialState: State =
    val emptyState = State(0, 0, Map.empty, Set.empty, false, rootCtx)
    val initScript = rootCtx.settings.replInitScript.value(using rootCtx)
    val combinedScript = initScript.trim() match
      case "" => extraPredef
      case script => s"$extraPredef\n$script"
    run(combinedScript)(using emptyState)

  /** Reset state of repl to the initial state
   *
   *  This method is responsible for performing an all encompassing reset. As
   *  such, when the user enters `:reset` this method should be called to reset
   *  everything properly
   */
  protected def resetToInitial(settings: List[String] = Nil): Unit = {
    rootCtx = initialCtx(settings)
    if (rootCtx.settings.outputDir.isDefault(using rootCtx))
      rootCtx = rootCtx.fresh
        .setSetting(rootCtx.settings.outputDir, new VirtualDirectory("<REPL compilation output>"))
    compiler = new ReplCompiler
    rendering = new Rendering(classLoader)
  }

  private var rootCtx: Context = uninitialized
  private var shouldStart: Boolean = uninitialized
  private var compiler: ReplCompiler = uninitialized
  protected var rendering: Rendering = uninitialized

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  override protected def command: CompilerCommand = ReplCommand

  /** Try to run REPL if there is nothing that prevents us doing so.
   *
   *  Possible reason for unsuccessful run are raised flags in CLI like --help or --version
   */
  final def tryRunning = if shouldStart then
    if rootCtx.settings.replQuitAfterInit.value(using rootCtx) then initialState
    else runUntilQuit()

  /** Run REPL with `state` until `:quit` command found
   *
   *  This method is the main entry point into the REPL. Its effects are not
   *  observable outside of the CLI, for this reason, most helper methods are
   *  `protected final` to facilitate testing.
   */
  def runUntilQuit(using initialState: State = initialState)(): State = {
    val terminal = new JLineTerminal

    out.println(
      s"""Welcome to Scala $simpleVersionString ($javaVersion, Java $javaVmName).
         |Type in expressions for evaluation. Or try :help.""".stripMargin)

    /** Blockingly read a line, getting back a parse result */
    def readLine()(using state: State): ParseResult = {
      given Context = state.context
      val completer: Completer = { (lineReader, line, candidates) =>
        def makeCandidate(label: String) = {
          new Candidate(
            /* value    = */ label,
            /* displ    = */ stripBackTicks(label), // displayed value
            /* group    = */ null,  // can be used to group completions together
            /* descr    = */ null,  // TODO use for documentation?
            /* suffix   = */ null,
            /* key      = */ null,
            /* complete = */ false  // if true adds space when completing
          )
        }
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.map(_.label).distinct.map(makeCandidate).asJava)
        val lineWord = line.word()
        comps.filter(c => c.label == lineWord && c.symbols.nonEmpty) match
          case Nil =>
          case exachMatches =>
            val terminal = lineReader.nn.getTerminal
            lineReader.callWidget(LineReader.CLEAR)
            terminal.writer.println()
            exachMatches.foreach: exact =>
              exact.symbols.foreach: sym =>
                terminal.writer.println(SyntaxHighlighting.highlight(sym.showUser))
            lineReader.callWidget(LineReader.REDRAW_LINE)
            lineReader.callWidget(LineReader.REDISPLAY)
            terminal.flush()
      }

      try {
        val line = terminal.readLine(completer)
        ParseResult(line)
      } catch {
        case _: EndOfFileException => // Ctrl+D
          Quit
        case _: UserInterruptException => // Ctrl+C at prompt - clear and continue
          SigKill
      }
    }

    @tailrec def loop(using state: State)(): State = {

      val res = readLine()
      if (res == Quit) state
      // Ctrl-C pressed at prompt - just continue with same state (line is cleared by JLine)
      else if (res == SigKill) loop(using state)()
      else {
        // Set up interrupt handler for command execution
        var firstCtrlCEntered = false
        val thread = Thread.currentThread()

        // Clear the stop flag before executing new code
        ReplBytecodeInstrumentation.setStopFlag(rendering.classLoader()(using state.context), false)

        val newState = terminal.withMonitoringCtrlC(
          handler = () =>
            if (!firstCtrlCEntered) {
              firstCtrlCEntered = true
              // Set the stop flag to trigger throwIfReplStopped() in instrumented code
              ReplBytecodeInstrumentation.setStopFlag(rendering.classLoader()(using state.context), true)
              // Also interrupt the thread as a fallback for non-instrumented code, e.g. IO/sleeps
              thread.interrupt()
              out.println("\nAttempting to interrupt running REPL command")
            } else {
              out.println("\nTerminating REPL Process...")
              System.exit(130)  // Standard exit code for SIGINT
            }
        ) {
          interpret(res)
        }

        loop(using newState)()
      }
    }

    try runBody { loop() }
    finally terminal.close()
  }

  final def run(input: String)(using state: State): State = runBody {
    interpret(ParseResult.complete(input))
  }

  protected def runBody(body: => State): State = rendering.classLoader()(using rootCtx).asContext(withRedirectedOutput(body))

  // TODO: i5069
  final def bind(name: String, value: Any)(using state: State): State = state

  /**
   * Controls whether the `System.out` and `System.err` streams are set to the provided constructor parameter instance
   * of [[java.io.PrintStream]] during the execution of the repl. On by default.
   *
   * Disabling this can be beneficial when executing a repl instance inside a concurrent environment, for example a
   * thread pool (such as the Scala compile server in the Scala Plugin for IntelliJ IDEA).
   *
   * In such environments, indepently executing `System.setOut` and `System.setErr` without any synchronization can
   * lead to unpredictable results when restoring the original streams (dependent on the order of execution), leaving
   * the Java process in an inconsistent state.
   */
  protected def redirectOutput: Boolean = true

  // redirecting the output allows us to test `println` in scripted tests
  private def withRedirectedOutput(op: => State): State = {
    if redirectOutput then
      val savedOut = System.out
      val savedErr = System.err
      try {
        System.setOut(out)
        System.setErr(out)
        op
      }
      finally {
        System.setOut(savedOut)
        System.setErr(savedErr)
      }
    else op
  }

  private def newRun(state: State, reporter: StoreReporter = newStoreReporter) = {
    val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
    state.copy(context = run.runContext)
  }

  private def stripBackTicks(label: String) =
    if label.startsWith("`") && label.endsWith("`") then
      label.drop(1).dropRight(1)
    else
      label

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected final def completions(cursor: Int, expr: String, state0: State): List[Completion] =
    if expr.startsWith(":") then
      ParseResult.commands.collect {
        case command if command._1.startsWith(expr) => Completion(command._1, "", List())
      }
    else
      given state: State = newRun(state0)
      compiler
        .typeCheck(expr, errorsAllowed = true)
        .map { (untpdTree, tpdTree) =>
          val file = SourceFile.virtual("<completions>", expr, maybeIncomplete = true)
          val unit = CompilationUnit(file)(using state.context)
          unit.untpdTree = untpdTree
          unit.tpdTree = tpdTree
          given Context = state.context.fresh.setCompilationUnit(unit)
          val srcPos = SourcePosition(file, Span(cursor))
          try Completion.completions(srcPos)._2 catch case NonFatal(_) => Nil
        }
        .getOrElse(Nil)
  end completions

  protected def interpret(res: ParseResult)(using state: State): State = {
    res match {
      case parsed: Parsed if parsed.source.content().mkString.startsWith("//>") =>
        // Check for magic comments specifying dependencies
        println("Please use `:dep com.example::artifact:version` to add dependencies in the REPL")
        state

      case parsed: Parsed if parsed.trees.nonEmpty =>
          compile(parsed, state)

      case SyntaxErrors(_, errs, _) =>
        displayErrors(errs)
        state

      case cmd: Command =>
        interpretCommand(cmd)

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
  }

  /** Compile `parsed` trees and evolve `state` in accordance */
  private def compile(parsed: Parsed, istate: State): State = {
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    def extractTopLevelImports(ctx: Context): List[tpd.Import] =
      unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

    def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
      if imports.isEmpty then ctx
      else
        imports.foldLeft(ctx.fresh.setNewScope)((ctx, imp) =>
          ctx.importContext(imp, imp.symbol(using ctx)))

    given State = {
      val state0 = newRun(istate, parsed.reporter)
      state0.copy(context = state0.context.withSource(parsed.source))
    }
    compiler
      .compile(parsed)
      .fold(
        displayErrors,
        {
          case (unit: CompilationUnit, newState: State) =>
            val newestWrapper = extractNewestWrapper(unit.untpdTree)
            val newImports = extractTopLevelImports(newState.context)
            var allImports = newState.imports
            if (newImports.nonEmpty)
              allImports += (newState.objectIndex -> newImports)
            val newStateWithImports = newState.copy(
              imports = allImports,
              context = contextWithNewImports(newState.context, newImports)
            )

            val warnings = newState.context.reporter
              .removeBufferedMessages(using newState.context)

            inContext(newState.context) {
              val (updatedState, definitions) =
                if (!ctx.settings.XreplDisableDisplay.value)
                  renderDefinitions(unit.tpdTree, newestWrapper)(using newStateWithImports)
                else
                  (newStateWithImports, Seq.empty)

              // output is printed in the order it was put in. warnings should be
              // shown before infos (eg. typedefs) for the same line. column
              // ordering is mostly to make tests deterministic
              given Ordering[Diagnostic] =
                Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

              (if istate.quiet then warnings else definitions ++ warnings)
                .sorted
                .foreach(printDiagnostic)

              updatedState
            }
        }
      )
  }

  private def renderDefinitions(tree: tpd.Tree, newestWrapper: Name)(using state: State): (State, Seq[Diagnostic]) = {
    given Context = state.context

    def resAndUnit(denot: Denotation) = {
      import scala.util.{Success, Try}
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match {
        case Success(num) => num < state.valIndex
        case _ => false
      }
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType
    }

    def extractAndFormatMembers(symbol: Symbol): (State, Seq[Diagnostic]) = if (tree.symbol.info.exists) {
      val info = symbol.info
      val defs =
        info.bounds.hi.finalResultType
          .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
          .filterNot { denot =>
            defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
             || denot.symbol.name.is(DefaultGetterName)
          }

      val vals =
        info.fields
          .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
          .filter(_.symbol.name.is(SimpleNameKind))

      val typeAliases =
        info.bounds.hi.typeMembers.filter(_.symbol.info.isTypeAlias)

      // The wrapper object may fail to initialize if the rhs of a ValDef throws.
      // In that case, don't attempt to render any subsequent vals, and mark this
      // wrapper object index as invalid.
      var failedInit = false
      val renderedVals =
        val buf = mutable.ListBuffer[Diagnostic]()
        for d <- vals do if !failedInit then rendering.renderVal(d) match
          case Right(Some(v)) =>
            buf += v
          case Left(e) =>
            buf += rendering.renderError(e, d)
            failedInit = true
          case _ =>
        buf.toList

      if failedInit then
        // We limit the returned diagnostics here to `renderedVals`, which will contain the rendered error
        // for the val which failed to initialize. Since any other defs, aliases, imports, etc. from this
        // input line will be inaccessible, we avoid rendering those so as not to confuse the user.
        (state.copy(invalidObjectIndexes = state.invalidObjectIndexes + state.objectIndex), renderedVals)
      else
        val formattedMembers =
          typeAliases.map(rendering.renderTypeAlias)
          ++ defs.map(rendering.renderMethod)
          ++ renderedVals
        val diagnostics = if formattedMembers.isEmpty then rendering.forceModule(symbol) else formattedMembers
        (state.copy(valIndex = state.valIndex - vals.count(resAndUnit)), diagnostics)
    }
    else (state, Seq.empty)

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol): Seq[Diagnostic] = sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          rendering.renderTypeDef(x)
      }

    atPhase(typerPhase.next) {
      // Display members of wrapped module:
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          val (newState, formattedMembers) = extractAndFormatMembers(wrapperModule.symbol)
          val formattedTypeDefs =  // don't render type defs if wrapper initialization failed
            if newState.invalidObjectIndexes.contains(state.objectIndex) then Seq.empty
            else typeDefs(wrapperModule.symbol)
          (newState, formattedTypeDefs ++ formattedMembers)
        }
        .getOrElse {
          // user defined a trait/class/object, so no module needed
          (state, Seq.empty)
        }
    }
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command)(using state: State): State = cmd match {
    case UnknownCommand(cmd) =>
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state

    case AmbiguousCommand(cmd, matching) =>
      out.println(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      state

    case Help =>
      out.println(Help.text)
      state

    case Reset(arg) =>
      val tokens = tokenize(arg)

      if tokens.nonEmpty then
        out.println(s"""|Resetting REPL state with the following settings:
                        |  ${tokens.mkString("\n  ")}
                        |""".stripMargin)
      else
        out.println("Resetting REPL state.")

      resetToInitial(tokens)
      initialState

    case Imports =>
      for {
        objectIndex <- state.validObjectIndexes
        imp <- state.imports.getOrElse(objectIndex, Nil)
      } out.println(imp.show(using state.context))
      state

    case Load(path) =>
      val file = new JFile(path)
      if (file.exists) {
        val contents = Using(scala.io.Source.fromFile(file, StandardCharsets.UTF_8.name))(_.mkString).get
        run(contents)
      }
      else {
        out.println(s"""Couldn't find file "${file.getCanonicalPath}"""")
        state
      }

    case Require(path) =>
      out.println(":require is no longer supported, but has been replaced with :jar. Please use :jar")
      state

    case JarCmd(path) =>
      val jarFile = AbstractFile.getDirectory(path)
      if (jarFile == null)
        out.println(s"""Cannot add "$path" to classpath.""")
        state
      else
        def flatten(f: AbstractFile): Iterator[AbstractFile] =
          if (f.isClassContainer) f.iterator.flatMap(flatten)
          else Iterator(f)

        def tryClassLoad(classFile: AbstractFile): Option[String] = {
          val input = classFile.input
          try {
            val reader = new ClassReader(input)
            val clsName = reader.getClassName.replace('/', '.')
            rendering.myClassLoader.loadClass(clsName)
            Some(clsName)
          } catch
            case _: ClassNotFoundException => None
          finally {
            input.close()
          }
        }

        try {
          val entries = flatten(jarFile)

          val existingClass = entries.filter(_.ext.isClass).find(tryClassLoad(_).isDefined)
          if (existingClass.nonEmpty)
            out.println(s"The path '$path' cannot be loaded, it contains a classfile that already exists on the classpath: ${existingClass.get}")
          else inContext(state.context):
            val jarClassPath = ClassPathFactory.newClassPath(jarFile)
            val prevOutputDir = ctx.settings.outputDir.value

            // add to compiler class path
            ctx.platform.addToClassPath(jarClassPath)
            SymbolLoaders.mergeNewEntries(defn.RootClass, ClassPath.RootPackage, jarClassPath, ctx.platform.classPath)

            // new class loader with previous output dir and specified jar
            val prevClassLoader = rendering.classLoader()
            val jarClassLoader = fromURLsParallelCapable(
              jarClassPath.asURLs, prevClassLoader)
            rendering.myClassLoader = new AbstractFileClassLoader(
              prevOutputDir,
              jarClassLoader,
              AbstractFileClassLoader.InterruptInstrumentation.fromString(ctx.settings.XreplInterruptInstrumentation.value)
            )

            out.println(s"Added '$path' to classpath.")
        } catch {
          case e: Throwable =>
            out.println(s"Failed to load '$path' to classpath: ${e.getMessage}")
        }
        state

    case KindOf(expr) =>
      out.println(s"""The :kind command is not currently supported.""")
      state
    case TypeOf(expr) =>
      expr match {
        case "" => out.println(s":type <expression>")
        case _  =>
          compiler.typeOf(expr)(using newRun(state)).fold(
            displayErrors,
            res => out.println(res)  // result has some highlights
          )
      }
      state

    case DocOf(expr) =>
      expr match {
        case "" => out.println(s":doc <expression>")
        case _  =>
          compiler.docOf(expr)(using newRun(state)).fold(
            displayErrors,
            res => out.println(res)
          )
      }
      state

    case Sh(expr) =>
      out.println(s"""The :sh command is deprecated. Use `import scala.sys.process._` and `"command".!` instead.""")
      state

    case Settings(arg) => arg match
      case "" =>
        given ctx: Context = state.context
        for (s <- ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name))
          out.println(s"${s.name} = ${if s.value == "" then "\"\"" else s.value}")
        state
      case _  =>
        rootCtx = setupRootCtx(tokenize(arg).toArray, rootCtx)
        state.copy(context = rootCtx)

    case Silent => state.copy(quiet = !state.quiet)
    case Dep(dep) =>
      val depStrings = List(dep)
      if depStrings.nonEmpty then
        val deps = depStrings.flatMap(DependencyResolver.parseDependency)
        if deps.nonEmpty then
          DependencyResolver.resolveDependencies(deps) match
            case Right(files) =>
              if files.nonEmpty then
                inContext(state.context):
                  // Update both compiler classpath and classloader
                  val prevOutputDir = ctx.settings.outputDir.value
                  val prevClassLoader = rendering.classLoader()
                  rendering.myClassLoader = DependencyResolver.addToCompilerClasspath(
                    files,
                    prevClassLoader,
                    prevOutputDir
                  )
                  out.println(s"Resolved ${deps.size} dependencies (${files.size} JARs)")
            case Left(error) =>
              out.println(s"Error resolving dependencies: $error")
      state

    case Quit =>
      // end of the world!
      state
  }

  /** shows all errors nicely formatted */
  private def displayErrors(errs: Seq[Diagnostic])(using state: State): State = {
    errs.foreach(printDiagnostic)
    state
  }

  /** Like ConsoleReporter, but without file paths, -Xprompt displaying,
   *  and using a PrintStream rather than a PrintWriter so messages aren't re-encoded. */
  private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter {
    override def posFileStr(pos: SourcePosition) = "" // omit file paths
    override def printMessage(msg: String): Unit = out.println(msg)
    override def echoMessage(msg: String): Unit  = printMessage(msg)
    override def flush()(using Context): Unit    = out.flush()
  }

  /** Print warnings & errors using ReplConsoleReporter, and info straight to out */
  private def printDiagnostic(dia: Diagnostic)(using state: State) = dia.level match
    case interfaces.Diagnostic.INFO => out.println(dia.msg) // print REPL's special info diagnostics directly to out
    case _                          => ReplConsoleReporter.doReport(dia)(using state.context)

end ReplDriver
object ReplDriver:
  def pprintImport = "import pprint.pprintln\n"