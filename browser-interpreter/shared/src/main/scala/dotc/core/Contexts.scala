package dotc.core

import Names._
import Types._
import Symbols._

import scala.collection.mutable
import dotc.util.SourceFile

/**
 * Cross-platform context representation for the browser compiler.
 */
object Contexts {

  /** The compiler context */
  class Context {
    var owner: Symbol = NoSymbol
    var scope: Scope = Scope.empty
    var outer: Context | Null = null
    var source: SourceFile = SourceFile.NoSource
    var compilationUnit: CompilationUnit | Null = null
    var reporter: Reporter = new Reporter
    var settings: Settings = new Settings
    var typerState: TyperState = new TyperState
    var phaseId: Int = 0

    def fresh: FreshContext = {
      val ctx = new FreshContext
      ctx.outer = this
      ctx.owner = owner
      ctx.scope = scope
      ctx.source = source
      ctx.compilationUnit = compilationUnit
      ctx.reporter = reporter
      ctx.settings = settings
      ctx.typerState = typerState
      ctx.phaseId = phaseId
      ctx
    }

    def error(msg: String, pos: dotc.util.SourcePosition): Unit =
      reporter.error(msg, pos)

    def warning(msg: String, pos: dotc.util.SourcePosition): Unit =
      reporter.warning(msg, pos)

    def lookupType(name: TypeName): Type = {
      val sym = scope.lookup(name)
      if (sym.exists) sym.info else NoType
    }

    def lookupTerm(name: TermName): Symbol = scope.lookup(name)

    def enter(sym: Symbol): Unit = {
      sym.owner = owner
      scope.enter(sym)
    }

    def definitions: Types.defn.type = Types.defn
  }

  class FreshContext extends Context {
    def setOwner(owner: Symbol): this.type = { this.owner = owner; this }
    def setScope(scope: Scope): this.type = { this.scope = scope; this }
    def setSource(source: SourceFile): this.type = { this.source = source; this }
    def setCompilationUnit(unit: CompilationUnit): this.type = { this.compilationUnit = unit; this }
    def setTyperState(state: TyperState): this.type = { this.typerState = state; this }
  }

  class CompilationUnit(val source: SourceFile) {
    var untpdTree: dotc.ast.Trees.Tree = dotc.ast.Trees.EmptyTree
    var tpdTree: dotc.ast.Trees.Tree = dotc.ast.Trees.EmptyTree
  }

  class Settings {
    var debug: Boolean = false
    var verbose: Boolean = false
    var classpath: String = ""
    var outputDirectory: String = "."
  }

  class TyperState {
    var constraint: Constraint = new Constraint
    def fresh: TyperState = {
      val ts = new TyperState
      ts.constraint = constraint.clone()
      ts
    }
  }

  class Constraint extends Cloneable {
    private val entries = mutable.HashMap[TypeSymbol, TypeBounds]()
    def add(param: TypeSymbol, bounds: TypeBounds): Unit = entries(param) = bounds
    def bounds(param: TypeSymbol): TypeBounds = entries.getOrElse(param, TypeBounds(NoType, NoType))
    def contains(param: TypeSymbol): Boolean = entries.contains(param)
    def entry(param: TypeParamRef): Type = NoType
    override def clone(): Constraint = {
      val c = new Constraint
      c.entries ++= entries
      c
    }
  }

  case class TypeParamRef(binder: PolyType, paramNum: Int) extends Type

  class Reporter {
    private val errors = mutable.ListBuffer[(String, dotc.util.SourcePosition)]()
    private val warnings = mutable.ListBuffer[(String, dotc.util.SourcePosition)]()
    def error(msg: String, pos: dotc.util.SourcePosition): Unit = errors += ((msg, pos))
    def warning(msg: String, pos: dotc.util.SourcePosition): Unit = warnings += ((msg, pos))
    def hasErrors: Boolean = errors.nonEmpty
    def errorCount: Int = errors.size
    def warningCount: Int = warnings.size
    def getErrors: List[(String, dotc.util.SourcePosition)] = errors.toList
    def getWarnings: List[(String, dotc.util.SourcePosition)] = warnings.toList
    def reset(): Unit = { errors.clear(); warnings.clear() }
  }

  def initialContext: Context = {
    val ctx = new FreshContext
    ctx.owner = rootPackage
    ctx.scope = rootPackage.decls
    ctx
  }

  lazy val rootPackage: PackageSymbol = {
    val pkg = new PackageSymbol(termName("<root>"))
    pkg.owner = NoSymbol
    pkg
  }

  lazy val emptyPackage: PackageSymbol = {
    val pkg = new PackageSymbol(termName("<empty>"))
    pkg.owner = rootPackage
    rootPackage.enter(pkg)
    pkg
  }

  type Ctx = Context
  inline def ctx(using c: Context): Context = c
}

