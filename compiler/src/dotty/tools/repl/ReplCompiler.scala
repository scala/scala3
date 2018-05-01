package dotty.tools
package repl

import dotc.ast.Trees._
import dotc.ast.{ untpd, tpd }
import dotc.{ Run, CompilationUnit, Compiler }
import dotc.core.Decorators._, dotc.core.Flags._, dotc.core.Phases, Phases.Phase
import dotc.core.Names._, dotc.core.Contexts._, dotc.core.StdNames._
import dotc.core.Constants.Constant
import dotc.util.SourceFile
import dotc.typer.{ ImportInfo, FrontEnd }
import backend.jvm.GenBCode
import dotc.core.NameOps._
import dotc.util.Positions._
import dotc.reporting.diagnostic.messages
import io._

import results._

/** This subclass of `Compiler` replaces the appropriate phases in order to
 *  facilitate the REPL
 *
 *  Specifically it replaces the front end with `REPLFrontEnd`, and adds a
 *  custom subclass of `GenBCode`. The custom `GenBCode`, `REPLGenBCode`, works
 *  in conjunction with a specialized class loader in order to load virtual
 *  classfiles.
 */
class ReplCompiler(val directory: AbstractFile) extends Compiler {


  /** A GenBCode phase that outputs to a virtual directory */
  private class REPLGenBCode extends GenBCode {
    override def phaseName = "genBCode"
    override def outputDir(implicit ctx: Context) = directory
  }

  override protected def frontendPhases: List[List[Phase]] =
    Phases.replace(classOf[FrontEnd], _ => new REPLFrontEnd :: Nil, super.frontendPhases)

  override protected def backendPhases: List[List[Phase]] =
    List(new REPLGenBCode) :: Nil

  def newRun(initCtx: Context, objectIndex: Int) = new Run(this, initCtx) {
    override protected[this] def rootContext(implicit ctx: Context) =
      addMagicImports(super.rootContext.fresh.setReporter(storeReporter))

    private def addMagicImports(initCtx: Context): Context = {
      def addImport(path: TermName)(implicit ctx: Context) = {
        val importInfo = ImportInfo.rootImport { () =>
          ctx.requiredModuleRef(path)
        }
        ctx.fresh.setNewScope.setImportInfo(importInfo)
      }

      (1 to objectIndex)
        .foldLeft(initCtx) { (ictx, i) =>
          addImport(nme.EMPTY_PACKAGE ++ "." ++ objectNames(i))(ictx)
        }
    }
  }

  private[this] var objectNames = Map.empty[Int, TermName]
  private def objectName(state: State) =
    objectNames.get(state.objectIndex).getOrElse {
      val newName = (str.REPL_SESSION_LINE + state.objectIndex).toTermName
      objectNames = objectNames + (state.objectIndex -> newName)
      newName
    }

  private case class Definitions(stats: List[untpd.Tree], state: State)

  private def definitions(trees: List[untpd.Tree], state: State): Definitions = {
    import untpd._

    implicit val ctx: Context = state.run.runContext

    var valIdx = state.valIndex

    val defs = trees.flatMap {
      case expr @ Assign(id: Ident, rhs) =>
        // special case simple reassignment (e.g. x = 3)
        // in order to print the new value in the REPL
        val assignName = (id.name ++ str.REPL_ASSIGN_SUFFIX).toTermName
        val assign = ValDef(assignName, TypeTree(), id).withPos(expr.pos)
        List(expr, assign)
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val vd = ValDef(resName, TypeTree(), expr).withPos(expr.pos)
        vd :: Nil
      case other =>
        other :: Nil
    }

    Definitions(
      state.imports ++ defs,
      state.copy(
        objectIndex = state.objectIndex + (if (defs.isEmpty) 0 else 1),
        valIndex = valIdx
      )
    )
  }

  /** Wrap trees in an object and add imports from the previous compilations
   *
   *  The resulting structure is something like:
   *
   *  ```
   *  package <none> {
   *    object rs$line$nextId {
   *      import rs$line${i <- 0 until nextId}._
   *
   *      <trees>
   *    }
   *  }
   *  ```
   */
  private def wrapped(defs: Definitions): untpd.PackageDef = {
    import untpd._

    assert(defs.stats.nonEmpty)

    implicit val ctx: Context = defs.state.run.runContext

    val tmpl = Template(emptyConstructor, Nil, EmptyValDef, defs.stats)
    val module = ModuleDef(objectName(defs.state), tmpl)
      .withMods(new Modifiers(Module | Final))
      .withPos(Position(0, defs.stats.last.pos.end))

    PackageDef(Ident(nme.EMPTY_PACKAGE), List(module))
  }

  private def createUnit(defs: Definitions, sourceCode: String): CompilationUnit = {
    val unit = new CompilationUnit(new SourceFile(objectName(defs.state).toString, sourceCode))
    unit.untpdTree = wrapped(defs)
    unit
  }

  private def runCompilationUnit(unit: CompilationUnit, state: State): Result[(CompilationUnit, State)] = {
    val run = state.run
    val reporter = state.run.runContext.reporter
    run.compileUnits(unit :: Nil)

    if (!reporter.hasErrors) (unit, state).result
    else run.runContext.flushBufferedMessages().errors
  }

  def compile(parsed: Parsed)(implicit state: State): Result[(CompilationUnit, State)] = {
    val defs = definitions(parsed.trees, state)
    val unit = createUnit(defs, parsed.sourceCode)
    runCompilationUnit(unit, defs.state)
  }

  def typeOf(expr: String)(implicit state: State): Result[String] =
    typeCheck(expr).map { tree =>
      import dotc.ast.Trees._
      implicit val ctx = state.run.runContext
      tree.rhs match {
        case Block(xs, _) => xs.last.tpe.widen.show
        case _ =>
          """Couldn't compute the type of your expression, so sorry :(
            |
            |Please report this to my masters at github.com/lampepfl/dotty
          """.stripMargin
      }
    }

  def typeCheck(expr: String, errorsAllowed: Boolean = false)(implicit state: State): Result[tpd.ValDef] = {

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(implicit ctx: Context): Result[untpd.PackageDef] = {
      def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef =
          ValDef("expr".toTermName, TypeTree(), Block(trees.toList, untpd.unitLiteral))

        val tmpl = Template(emptyConstructor,
                            List(Ident(tpnme.Any)),
                            EmptyValDef,
                            state.imports :+ valdef)

        PackageDef(Ident(nme.EMPTY_PACKAGE),
          TypeDef("EvaluateExpr".toTypeName, tmpl)
            .withMods(new Modifiers(Final))
            .withPos(Position(0, expr.length)) :: Nil
        )
      }

      ParseResult(expr) match {
        case Parsed(sourceCode, trees) =>
          wrap(trees).result
        case SyntaxErrors(_, reported, trees) =>
          if (errorsAllowed) wrap(trees).result
          else reported.errors
        case _ => List(
          new messages.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atPos(Position(0, expr.length))
          )
        ).errors
      }
    }

    def unwrapped(tree: tpd.Tree, sourceFile: SourceFile)(implicit ctx: Context): Result[tpd.ValDef] = {
      def error: Result[tpd.ValDef] =
        List(new messages.Error(s"Invalid scala expression",
          sourceFile.atPos(Position(0, sourceFile.content.length)))).errors

      import tpd._
      tree match {
        case PackageDef(_, List(TypeDef(_, tmpl: Template))) =>
          tmpl.body
              .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd.result }
              .getOrElse(error)
        case _ =>
          error
      }
    }


    val run = state.run
    val reporter = state.run.runContext.reporter
    val src = new SourceFile(s"EvaluateExpr", expr)
    val runCtx =
      run.runContext.fresh
         .setSetting(run.runContext.settings.YstopAfter, List("frontend"))

    wrapped(expr, src, state)(runCtx).flatMap { pkg =>
      val unit = new CompilationUnit(src)
      unit.untpdTree = pkg
      run.compileUnits(unit :: Nil, runCtx.fresh.setReporter(storeReporter))

      if (errorsAllowed || !reporter.hasErrors)
        unwrapped(unit.tpdTree, src)(runCtx)
      else {
        reporter.removeBufferedMessages(runCtx).errors
      }
    }
  }
}
