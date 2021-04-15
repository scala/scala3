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
import reporting._
import config.Printers.init
import ast.tpd._

import Errors._, Potentials._, Effects._

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
 *
 *  Note: Virtual method resolution should have been performed for the target.
 *
 */
case class StaticCall(cls: ClassSymbol, symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "StaticCall(" + cls.show + ", " + symbol.show + ")"
}

/** A static method call result is used
 *
 *  Note: Virtual method resolution should have been performed for the target.
 */
 case class ProxyUsage(cls: ClassSymbol, symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "ProxyUsage(" + cls.show + ", " + symbol.show + ")"
}

/** A class is used
 *
 *  This is a coarse-grained abstraction
 */
case class ClassUsage(symbol: Symbol)(val source: Tree) extends Dependency {
  def show(using Context): String = "ClassUsage(" + symbol.show + ")"
}


class CycleChecker(cache: Cache) {
  private val summaryCache = mutable.Map.empty[Symbol, List[Dependency]]
  private val proxyCache = mutable.Map.empty[Symbol, List[Dependency]]

  val classesInCurrentRun = mutable.Set.empty[Symbol]
  val objectsInCurrentRun = mutable.Set.empty[Symbol]

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
  def checkCyclic()(using Context): Unit = {
    val state = State(visited = mutable.Set.empty, path = Vector.empty, trace = Vector.empty)
    objectsInCurrentRun.foreach { obj =>
      val dep = ObjectAccess(obj)(obj.defTree)
      val errors = check(dep)(using ctx, state)
      errors.foreach(_.issue)
    }
  }

  private def check(dep: Dependency)(using Context, State): List[Error] =
    trace("checking dependency " + dep.show, init, errs => Errors.show(errs.asInstanceOf[Errors])) {
      dep match
      case dep: ObjectAccess   => checkObjectAccess(dep)
      case _ =>
        if state.visited.contains(dep) then Nil
        else state.visit(dep) {
          dep match
          case dep: InstanceUsage  => checkInstanceUsage(dep)
          case dep: StaticCall     => checkStaticCall(dep)
          case dep: ProxyUsage     => checkProxyUsage(dep)
          case dep: ClassUsage     => checkClassUsage(dep)
        }
    }

  private def checkObjectAccess(dep: ObjectAccess)(using Context, State): List[Error] =
    if !objectsInCurrentRun.contains(dep.symbol) then
      Util.traceIndented("skip " + dep.symbol.show + " which is not in current run ", init)
      Nil
    else {
      Util.traceIndented("state.path = " + state.path.map(_.show), init)
      val obj = dep.symbol
      if state.path.contains(obj) then
        val cycle = state.path.dropWhile(_ != obj)
        val trace = state.trace.map(_.source) :+ dep.source
        if cycle.size > 1 then
          CyclicObjectInit(obj, trace) :: Nil
        else
          ObjectLeakDuringInit(obj, trace) :: Nil
      else
        val constr = obj.moduleClass.primaryConstructor
        state.withPath(obj) {
          check(StaticCall(constr.owner.asClass, constr)(dep.source))
        }
    }


  private def checkInstanceUsage(dep: InstanceUsage)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.symbol) then
      Util.traceIndented("skip " + dep.symbol.show + " which is not in current run ", init)
      Nil
    else {
      val cls = dep.symbol
      val deps = classDependencies(cls, excludeInit = true)
      deps.flatMap(check(_))
    }

  private def checkStaticCall(dep: StaticCall)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.cls) || !classesInCurrentRun.contains(dep.symbol.owner)  then
      Util.traceIndented("skip " + dep.show + " which is not in current run ", init)
      Nil
    else {
      val deps = methodDependencies(dep)
      deps.flatMap(check(_))
    }

  private def checkProxyUsage(dep: ProxyUsage)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.cls) || !classesInCurrentRun.contains(dep.symbol.owner) then
      Util.traceIndented("skip " + dep.show + " which is not in current run ", init)
      Nil
    else {
      val deps = proxyDependencies(dep)
      deps.flatMap(check(_))
    }

  private def checkClassUsage(dep: ClassUsage)(using Context, State): List[Error] =
    if !classesInCurrentRun.contains(dep.symbol) then
      Util.traceIndented("skip " + dep.symbol.show + " which is not in current run ", init)
      Nil
    else {
      val cls = dep.symbol
      val deps = classDependencies(cls, excludeInit = false)
      deps.flatMap(check(_))
    }

// ----- analysis of dependencies -------------------------------

  def cacheConstructorDependencies(constr: Symbol, deps: List[Dependency])(using Context): Unit =
    Util.traceIndented("deps for " + constr.show + " = " + deps.map(_.show), init)
    summaryCache(constr) = deps
    val cls = constr.owner

    if cls.is(Flags.Module) && cls.isStatic then
      objectsInCurrentRun += cls.sourceModule

  private def classDependencies(sym: Symbol, excludeInit: Boolean)(using Context): List[Dependency] =
    if (summaryCache.contains(sym)) summaryCache(sym)
    else trace("summary for " + sym.show, init) {
      val cls = sym.asClass
      val deps = analyzeClass(cls, excludeInit)
      summaryCache(cls) = deps
      deps
    }

  private def methodDependencies(call: StaticCall)(using Context): List[Dependency] = trace("dependencies of " + call.symbol.show, init, _.asInstanceOf[List[Dependency]].map(_.show).toString) {
    if (summaryCache.contains(call.symbol)) summaryCache(call.symbol)
    else trace("summary for " + call.symbol.show) {
      val deps = analyzeMethod(call)
      summaryCache(call.symbol) = deps
      deps
    }
  }

  private def proxyDependencies(dep: ProxyUsage)(using Context): List[Dependency] = trace("dependencies of " + dep.symbol.show, init, _.asInstanceOf[List[Dependency]].map(_.show).toString) {
    if (proxyCache.contains(dep.symbol)) summaryCache(dep.symbol)
    else trace("summary for " + dep.symbol.show) {
      val env = Env(ctx.withOwner(dep.cls), cache)
      val state = new Checking.State(
        visited = Set.empty,
        path = Vector.empty,
        thisClass = dep.cls,
        fieldsInited = mutable.Set.empty,
        parentsInited = mutable.Set.empty,
        safePromoted = mutable.Set(ThisRef()(dep.cls.defTree)),
        dependencies = mutable.Set.empty,
        env = env
      ) {
        override def isFieldInitialized(field: Symbol): Boolean = true
      }

      val pot = Hot(dep.cls)(dep.source)
      val effs = pot.potentialsOf(dep.symbol)(using env).promote(dep.source)

      val errs = effs.flatMap(Checking.check(_)(using state))
      assert(errs.isEmpty, "unexpected errors: " + Errors.show(errs.toList))

      val deps = state.dependencies.toList
      proxyCache(dep.symbol) = deps
      deps
    }
  }

  def isStaticObjectRef(sym: Symbol)(using Context) =
    sym.isTerm && !sym.is(Flags.Package) && sym.is(Flags.Module) && sym.isStatic

  private def analyzeClass(cls: ClassSymbol, excludeInit: Boolean)(using Context): List[Dependency] = {
    val cdef = cls.defTree.asInstanceOf[TypeDef]
    val tpl = cdef.rhs.asInstanceOf[Template]

    var deps = new mutable.ListBuffer[Dependency]

    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(using Context): Unit =
        tree match {
          case tree @ Template(_, parents, self, _) =>
            if !excludeInit then
              parents.foreach(traverse(_))
            tree.body.foreach {
              case ddef: DefDef if !ddef.symbol.isConstructor =>
                traverse(ddef)
              case vdef: ValDef if vdef.symbol.is(Flags.Lazy) =>
                traverse(vdef)
              case stat =>
                if !excludeInit then traverse(stat)
            }

          case tree: RefTree if tree.isTerm =>
            deps ++= analyzeType(tree.tpe, tree, exclude = cls)

          case tree: This =>
            deps ++= analyzeType(tree.tpe, tree, exclude = cls)

          case tree: ValOrDefDef =>
            traverseChildren(tree.rhs)

          case tdef: TypeDef =>
            // don't go into nested classes

          case tree: New =>
            deps += ClassUsage(tree.tpe.classSymbol)(tree)

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

      deps += dep
    }

    traverser.traverse(tpl)
    deps.toList
  }

  private def analyzeType(tp: Type, source: Tree, exclude: Symbol)(using Context): List[Dependency] = tp match {
    case (_: ConstantType) | NoPrefix => Nil

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
      else
        Nil

    case SuperType(thisTp, _) =>
      analyzeType(thisTp, source, exclude)

    case _: TypeRef | _: AppliedType =>
      // possible from parent list
      Nil

    case _ =>
      throw new Exception("unexpected type: " + tp)
  }

  private def analyzeMethod(dep: StaticCall)(using Context): List[Dependency] = {
    val env = Env(ctx.withOwner(dep.cls), cache)
    val state = new Checking.State(
      visited = Set.empty,
      path = Vector.empty,
      thisClass = dep.cls,
      fieldsInited = mutable.Set.empty,
      parentsInited = mutable.Set.empty,
      safePromoted = mutable.Set(ThisRef()(dep.cls.defTree)),
      dependencies = mutable.Set.empty,
      env = env
    ) {
      override def isFieldInitialized(field: Symbol): Boolean = true
    }

    val pot = Hot(dep.cls)(dep.source)
    val effs = pot.effectsOf(dep.symbol)(using env)

    val errs = effs.flatMap(Checking.check(_)(using state))
    assert(errs.isEmpty, "unexpected errors: " + Errors.show(errs.toList))

    state.dependencies.toList
  }


// ----- cleanup ------------------------

  def clean() = {
    summaryCache.clear()
    classesInCurrentRun.clear()
    objectsInCurrentRun.clear()
  }
}
