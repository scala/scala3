package dotty.tools.dotc
package transform
package init

import core._
import Flags._
import Contexts._
import Types._
import Symbols._
import Decorators._
import printing.SyntaxHighlighting
import reporting.trace
import config.Printers.init

import ast.tpd._

import scala.collection.mutable


/** Check that static objects can be initialized without cycles
 *
 *  For the check to be fast, the algorithm uses coarse approximation.
 *  We construct a dependency graph as follows:
 *
 *  - if a static object `O` is used in another class/static-object `B`,
 *    then O -> B
 *  - if a class `C` is instantiated in a another class/static-object `B`,
 *    then C -> B
 *  - if a static-object/class `A` extends another class `B`,
 *    then A -> B
 *
 *  Given the graph above, we check if there exists cycles.
 *
 *  This check does not need to care about objects in libraries, as separate
 *  compilation ensures that there cannot be cyles between two separately
 *  compiled projects.
 */
class CheckGlobal {
  case class Dependency(sym: Symbol, source: Tree)

  /** Checking state */
  case class State(visited: mutable.Set[Symbol], path: Vector[Tree], obj: Symbol) {
    def cyclicPath(using Context): String = if (path.isEmpty) "" else " Cyclic path:\n" + {
      var indentCount = 0
      var last: String = ""
      val sb = new StringBuilder
      path.foreach { tree =>
        indentCount += 1
        val pos = tree.sourcePos
        val prefix = s"${ " " * indentCount }-> "
        val line =
          if pos.source.exists then
            val loc = "[ " + pos.source.file.name + ":" + (pos.line + 1) + " ]"
            val code = SyntaxHighlighting.highlight(pos.lineContent.trim)
            i"$code\t$loc"
          else
            tree.show

        if (last != line)  sb.append(prefix + line + "\n")

        last = line
      }
      sb.toString
    }
  }

  case class Error(state: State) {
    def issue(using Context): Unit =
      report.warning("Cylic object dependencies detected." + state.cyclicPath, state.obj.defTree.srcPos)
  }

  /** Summary of dependencies */
  private val summaryCache = mutable.Map.empty[Symbol, List[Dependency]]

  def check(obj: Symbol)(using Context): Unit = trace("checking " + obj.show, init) {
    checkDependencies(obj, State(visited = mutable.Set.empty, path = Vector.empty, obj)) match
    case Some(err) => err.issue
    case _ =>
  }

  private def check(sym: Symbol, state: State)(using Context): Option[Error] = trace("checking " + sym.show, init) {
    if sym == state.obj then
      Some(Error(state))
    else if state.visited.contains(sym) then
      None
    else
      state.visited += sym
      checkDependencies(sym, state)
  }

  private def checkDependencies(sym: Symbol, state: State)(using Context): Option[Error] = trace("checking dependencies of " + sym.show, init) {
    val cls = if sym.is(Module) then sym.moduleClass.asClass else sym.asClass
    val deps = analyze(cls)
    Util.traceIndented("dependencies of " + sym.show + " = " + deps.map(_.sym.show).mkString(","), init)
    var res: Option[Error] = None
    // TODO: stop early
    deps.foreach { dep =>
      if res.isEmpty then
        val state2: State = state.copy(path = state.path :+ dep.source)
        res = check(dep.sym, state2)
    }
    res
  }

  private def analyze(cls: ClassSymbol)(using Context): List[Dependency] =
    def isStaticObjectRef(sym: Symbol) =
      sym.isTerm && !sym.is(Package) && sym.is(Module)
      && sym.isStatic && sym.moduleClass != cls

    if (cls.defTree.isEmpty) Nil
    else if (summaryCache.contains(cls)) summaryCache(cls)
    else {
      val cdef = cls.defTree.asInstanceOf[TypeDef]
      val tpl = cdef.rhs.asInstanceOf[Template]
      var dependencies: List[Dependency] = Nil
      val traverser = new TreeTraverser {
        override def traverse(tree: Tree)(using Context): Unit =
          tree match {
            case tree: RefTree if isStaticObjectRef(tree.symbol) =>
              dependencies = Dependency(tree.symbol, tree) :: dependencies

            case tdef: TypeDef =>
              // don't go into nested classes

            case tree: New =>
              dependencies = Dependency(tree.tpe.classSymbol, tree) :: dependencies

            case _ =>
              traverseChildren(tree)
          }
      }

      def typeRefOf(tp: Type): TypeRef = tp.dealias.typeConstructor match {
        case tref: TypeRef => tref
        case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)
      }

      def addStaticOuterDep(tp: Type, source: Tree): Unit =
        tp match
        case NoPrefix =>
        case tmref: TermRef  =>
          if isStaticObjectRef(tmref.symbol) then
            dependencies = Dependency(tmref.symbol, source) :: dependencies
        case ThisType(tref) =>
          val obj = tref.symbol.sourceModule
          if isStaticObjectRef(obj) then
            dependencies = Dependency(obj, source) :: dependencies
        case _ =>
          throw new Exception("unexpected type: " + tp)

      // TODO: the traverser might create duplicate entries for parents
      tpl.parents.foreach { tree =>
        val tp = tree.tpe
        val tref = typeRefOf(tp)
        dependencies = Dependency(tp.classSymbol, tree) :: dependencies
        addStaticOuterDep(tref.prefix, tree)
      }

      traverser.traverse(tpl)
      summaryCache(cls) = dependencies
      dependencies
    }

  def debugCache(using Context) =
    summaryCache.map(_.show + " -> " + _.map(_.sym.show).mkString(",")).mkString("\n")
}