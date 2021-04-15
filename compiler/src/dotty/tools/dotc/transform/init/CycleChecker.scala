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

import Errors._

import scala.collection.mutable



/** The dependencies of a static object or a class
 *
 *  This class is used in checking cyclic initialization of static objects.
 *
 *  For the check to be simple and fast, the algorithm uses a combination of
 *  coarse-grained analysis and fine-grained analysis.
 *
 *  Fine-grained abstractions are created from the initialization
 *  check for static objects.
 *
 *  Coarse-grained abstractions are created as follows:
 *
 *  - if a static object `O` is used in another class/static-object `B`,
 *    then B -> O
 *  - if `new C` appears in a another class/static-object `B`,
 *    then B -> C
 *  - if a static-object/class `A` extends another class `B`,
 *    then A -> B
 *
 *  Given a dependency graph, we check if there exists cycles.
 *
 *  This check does not need to care about objects in libraries, as separate
 *  compilation ensures that there cannot be cyles between two separately
 *  compiled projects.
 *
 */
trait Dependency {
  def symbol: Symbol
  def source: Tree
  def show(using Context): String
}

/** Depend on the initialization of another object */
case class ObjectAccess(symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "ObjectAccess(" + symbol.show + ")"
}

/** Depend on usage of an instance, which can be either a class instance or object */
case class InstanceUsage(symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "InstanceUsage(" + symbol.show + ")"
}

/** A static method call detected from fine-grained analysis
 *
 *  The method can be either on a static object or on a hot object.
 *  The target of the call is determined statically.
 */
case class StaticCall(cls: Symbol, symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "StaticCall(" + cls.show + ", " + symbol.show + ")"
}

/** A class is used
 *
 *  This is a coarse-grained abstraction
 */
case class ClassUsage(symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "ClassUsage(" + symbol.show + ")"
}


class CycleChecker {
  private val summaryCache = mutable.Map.empty[Symbol, List[Dependency]]

  private val classesInCurrentRun = mutable.Set.empty[Symbol]
  private val objectsInCurrentRun = mutable.Set.empty[Symbol]

  /** Checking state */
  case class State(visited: mutable.Set[Dependency], path: Vector[Symbol], trace: Vector[Dependency]) {
    def visit[T](dep: Dependency)(op: State ?=> T): T =
      this.visited += dep
      val state2: State = this.copy(trace = trace :+ dep)
      val res = op(using state2)
      res

    def withPath[T](obj: Symbol)(op: State ?=> T): T =
      val state2 = this.copy(path = path :+ obj)
      val res = op(using state2)
      res

  }

  def state(using ev: State) = ev

// ----- checking -------------------------------
  def checkCyclic(): Unit = ???

  private def check(dep: Dependency)(using Context, State): List[Error] =
    trace("checking dependency " + dep.show, init, errs => Errors.show(errs.asInstanceOf[Errors])) {
      if state.visited.contains(dep) then
        Nil
      else
        state.visit(dep) {
          dep match
          case dep: ObjectAccess   => checkObjectAccess(dep)
          case dep: InstanceUsage  => checkInstanceUsage(dep)
          case dep: StaticCall     => checkStaticCall(dep)
          case dep: ClassUsage     => checkClassUsage(dep)
        }
    }

  private def checkObjectAccess(dep: ObjectAccess)(using Context, State): List[Error] =
    val obj = dep.symbol
    if state.path.contains(obj) then
      val cycle = state.path.dropWhile(_ != obj)
      if cycle.size > 1 then
        val trace = state.trace.map(_.source) :+ dep.source
        val error = CyclicObjectInit(obj, trace)
        error :: Nil
      else
        // TODO: issue a warning for access an object outside its scope during its initialization
        Nil
    else
      val constr = obj.primaryConstructor
      state.withPath(obj) {
        check(StaticCall(constr.owner, constr)(dep.source))
      }


  private def checkInstanceUsage(dep: InstanceUsage)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.symbol) then Nil
    else {
      val cls = dep.symbol
      val deps = classDependencies(cls, excludeInit = true)
      deps.flatMap(check(_))
    }

  private def checkStaticCall(dep: StaticCall)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.cls) then Nil
    else {
      val deps = methodDependencies(dep.cls, dep.symbol)
      deps.flatMap(check(_))
    }

  private def checkClassUsage(dep: ClassUsage)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.symbol) then Nil
    else {
      val cls = dep.symbol
      val deps = classDependencies(cls, excludeInit = false)
      deps.flatMap(check(_))
    }

// ----- analysis of dependencies -------------------------------

  def cacheConstructorDependencies(constr: Symbol, deps: List[Dependency])(using Context): Unit =
    summaryCache(constr) = deps
    val cls = constr.owner
    if cls.is(Flags.Module) && cls.isStatic then
      objectsInCurrentRun += cls.sourceModule
    else
      classesInCurrentRun += cls

  private def classDependencies(sym: Symbol, excludeInit: Boolean)(using Context): List[Dependency] =
    if (summaryCache.contains(sym)) summaryCache(sym)
    else trace("summary for " + sym.show, init) {
      val cls = sym.asClass
      val deps = analyzeClass(cls, excludeInit)
      summaryCache(cls) = deps
      deps
    }

  private def methodDependencies(cls: Symbol, sym: Symbol)(using Context): List[Dependency] =
    if (summaryCache.contains(sym)) summaryCache(sym)
    else trace("summary for " + sym.show) {
      val deps = analyzeMethod(cls, sym)
      summaryCache(sym) = deps
      deps
    }

  def isStaticObjectRef(sym: Symbol)(using Context) =
    sym.isTerm && !sym.is(Flags.Package) && sym.is(Flags.Module) && sym.isStatic

  private def analyzeClass(cls: ClassSymbol, excludeInit: Boolean)(using Context): List[Dependency] = {
    val cdef = cls.defTree.asInstanceOf[TypeDef]
    val tpl = cdef.rhs.asInstanceOf[Template]

    var deps: List[Dependency] = Nil

    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(using Context): Unit =
        tree match {
          case tree @ Template(_, parents, self, _) =>
            if !excludeInit then
              parents.foreach(traverse(_))
            tree.body.foreach {
              case ddef: DefDef if ddef.symbol.isConstructor =>
                traverse(ddef)
              case vdef: ValDef if vdef.symbol.is(Flags.Lazy) =>
                traverse(vdef)
              case stat =>
                if !excludeInit then traverse(stat)
            }

          case tree: RefTree if tree.isTerm =>
            analyzeType(tree.tpe, tree, exclude = cls)

          case tree: This =>
            analyzeType(tree.tpe, tree, exclude = cls)

          case tree: ValOrDefDef =>
            traverseChildren(tree.rhs)

          case tdef: TypeDef =>
            // don't go into nested classes

          case tree: New =>
            deps = ClassUsage(tree.tpe.classSymbol)(tree) :: deps

          case _ =>
            traverseChildren(tree)
        }
    }

    // TODO: the traverser might create duplicate entries for parents
    tpl.parents.foreach { tree =>
      val tp = tree.tpe
      val dep =
        if excludeInit then InstanceUsage(tp.classSymbol)(tree)
        else ClassUsage(tp.classSymbol)(tree)

      deps = dep :: deps
    }

    traverser.traverse(tpl)
    deps
  }

  private def analyzeType(tp: Type, source: Tree, exclude: Symbol)(using Context): Unit = tp match {
    case (_: ConstantType) | NoPrefix =>

    case tmref: TermRef if isStaticObjectRef(tmref.symbol) =>
      val obj = tmref.symbol
      ObjectAccess(obj)(source) :: InstanceUsage(obj.moduleClass)(source) :: Nil

    case tmref: TermRef =>
      analyzeType(tmref.prefix, source, exclude)

    case ThisType(tref) =>
      if isStaticObjectRef(tref.symbol.sourceModule) && tref.symbol != exclude
      then
        val cls = tref.symbol
        val obj = cls.sourceModule
        ObjectAccess(obj)(source) :: InstanceUsage(cls)(source) :: Nil

    case SuperType(thisTp, _) =>
      analyzeType(thisTp, source, exclude)

    case _: TypeRef | _: AppliedType =>
      // possible from parent list

    case _ =>
      throw new Exception("unexpected type: " + tp)
  }


  private def analyzeMethod(meth: Symbol)(using Context): List[Dependency] = ???

// ----- cleanup ------------------------

  def clean() = {
    summaryCache.clear()
    classesInCurrentRun.clear()
    objectsInCurrentRun.clear()
  }
}
