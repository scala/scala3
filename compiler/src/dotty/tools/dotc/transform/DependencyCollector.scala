package dotty.tools.dotc
package transform

import MegaPhase._
import core.Denotations.NonSymSingleDenotation
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import core.NameKinds.ExpandPrefixName
import ast.Trees._
import SymUtils._
import ExplicitOuter.outer
import util.Store
import collection.mutable
import collection.mutable.{ HashMap, HashSet, LinkedHashMap, TreeSet }
import annotation.constructorOnly

/** Exposes the dependencies of the typed tree in the current compilation unit
 *  in sets `freeVars`, `liftedOwner`.
 */
abstract class Dependencies(@constructorOnly rootContext: Context):
  import ast.tpd._

  protected def enclosure(using Context): Symbol
  protected def isExpr(sym: Symbol)(using Context): Boolean

  type SymSet = TreeSet[Symbol]

  /** A map storing free variables of functions and classes */
  private val free: mutable.LinkedHashMap[Symbol, SymSet] = new LinkedHashMap

  /** A hashtable storing calls between functions */
  private val called = new LinkedHashMap[Symbol, SymSet]

  /** A map from local methods and classes to the owners to which they will be lifted as members.
   *  For methods and classes that do not have any dependencies this will be the enclosing package.
   *  symbols with packages as lifted owners will subsequently represented as static
   *  members of their toplevel class, unless their enclosing class was already static.
   *  Note: During tree transform (which runs at phase LambdaLift + 1), liftedOwner
   *  is also used to decide whether a method had a term owner before.
   */
  private val depOwner = new LinkedHashMap[Symbol, Symbol]

  /** A flag to indicate whether new free variables have been found */
  private var changedFreeVars: Boolean = _

  /** A flag to indicate whether lifted owners have changed */
  private var changedLiftedOwner: Boolean = _

  private val ord: Ordering[Symbol] = Ordering.by(_.id)
  private def newSymSet = TreeSet.empty[Symbol](ord)

  private def symSet(f: LinkedHashMap[Symbol, SymSet], sym: Symbol): SymSet =
    f.getOrElseUpdate(sym, newSymSet)

  def freeVars(sym: Symbol): collection.Set[Symbol] = free.getOrElse(sym, Set.empty)

  def tracked: Iterable[Symbol] = free.keys

  def dependentOwner: collection.Map[Symbol, Symbol] = depOwner

  /** A symbol is local if it is owned by a term or a local trait,
   *  or if it is a constructor of a local symbol.
   *  Note: we count members of local traits as local since their free variables
   *  have to be passed on from their callers. By contrast, class members get their
   *  free variable proxies from their enclosing class.
   */
  private def isLocal(sym: Symbol)(using Context): Boolean =
    val owner = sym.maybeOwner
    owner.isTerm
    || owner.is(Trait) && isLocal(owner)
    || sym.isConstructor && isLocal(owner)

  /** Set `liftedOwner(sym)` to `owner` if `owner` is more deeply nested
   *  than the previous value of `liftedowner(sym)`.
   */
  private def narrowLiftedOwner(sym: Symbol, owner: Symbol)(using Context): Unit =
    if sym.maybeOwner.isTerm
        && owner.isProperlyContainedIn(depOwner(sym))
        && owner != sym
    then
      report.log(i"narrow lifted $sym to $owner")
      changedLiftedOwner = true
      depOwner(sym) = owner

  private class NoPath extends Exception

  /** Mark symbol `sym` as being free in `enclosure`, unless `sym` is defined
   *  in `enclosure` or there is an intermediate class properly containing `enclosure`
   *  in which `sym` is also free. Also, update `liftedOwner` of `enclosure` so
   *  that `enclosure` can access `sym`, or its proxy in an intermediate class.
   *  This means:
   *
   *    1. If there is an intermediate class in which `sym` is free, `enclosure`
   *       must be contained in that class (in order to access the `sym proxy stored
   *       in the class).
   *
   *    2. If there is no intermediate class, `enclosure` must be contained
   *       in the class enclosing `sym`.
   *
   *  @return  If there is a non-trait class between `enclosure` and
   *           the owner of `sym`, the largest such class.
   *           Otherwise, if there is a trait between `enclosure` and
   *           the owner of `sym`, the largest such trait.
   *           Otherwise, NoSymbol.
   *
   *  @pre sym.owner.isTerm, (enclosure.isMethod || enclosure.isClass)
   *
   *  The idea of `markFree` is illustrated with an example:
   *
   *  def f(x: int) = {
   *    class C {
   *      class D {
   *        val y = x
   *      }
   *    }
   *  }
   *
   *  In this case `x` is free in the primary constructor of class `C`.
   *  but it is not free in `D`, because after lambda lift the code would be transformed
   *  as follows:
   *
   *  def f(x$0: int) {
   *    class C(x$0: int) {
   *      val x$1 = x$0
   *      class D {
   *        val y = outer.x$1
   *      }
   *    }
   *  }
   */
  private def markFree(sym: Symbol, enclosure: Symbol)(using Context): Symbol = try {
    if (!enclosure.exists) throw new NoPath
    if (enclosure == sym.enclosure) NoSymbol
    else {
      def nestedInConstructor(sym: Symbol): Boolean =
        sym.isConstructor ||
        sym.isTerm && nestedInConstructor(sym.enclosure)
      report.debuglog(i"mark free: ${sym.showLocated} with owner ${sym.maybeOwner} marked free in $enclosure")
      val intermediate =
        if (enclosure.is(PackageClass)) enclosure
        else if (enclosure.isConstructor) markFree(sym, enclosure.owner.enclosure)
        else markFree(sym, enclosure.enclosure)
      if (intermediate.exists) narrowLiftedOwner(enclosure, intermediate)
      if !intermediate.isRealClass || nestedInConstructor(enclosure) then
        // Constructors and methods nested inside traits get the free variables
        // of the enclosing trait or class.
        // Conversely, local traits do not get free variables.
        // Methods inside constructors also don't have intermediates,
        // need to get all their free variables passed directly.
        if (!enclosure.is(Trait))
          if (symSet(free, enclosure).add(sym)) {
            changedFreeVars = true
            report.log(i"$sym is free in $enclosure")
          }
      if (intermediate.isRealClass) intermediate
      else if (enclosure.isRealClass) enclosure
      else if (intermediate.isClass) intermediate
      else if (enclosure.isClass) enclosure
      else NoSymbol
    }
  }
  catch {
    case ex: NoPath =>
      println(i"error lambda lifting ${ctx.compilationUnit}: $sym is not visible from $enclosure")
      throw ex
  }

  private def markCalled(callee: Symbol, caller: Symbol)(using Context): Unit = {
    report.debuglog(i"mark called: $callee of ${callee.owner} is called by $caller in ${caller.owner}")
    assert(isLocal(callee))
    symSet(called, caller) += callee
  }

  protected def process(tree: Tree)(using Context) =
    val sym = tree.symbol

    def narrowTo(thisClass: ClassSymbol) =
      val enclMethod = enclosure
      val enclClass = enclMethod.enclosingClass
      narrowLiftedOwner(enclMethod,
        if enclClass.isContainedIn(thisClass) then thisClass
        else enclClass) // unknown this reference, play it safe and assume the narrowest possible owner

    tree match
      case tree: Ident =>
        if isLocal(sym) then
          if isExpr(sym) then markCalled(sym, enclosure)
          else if sym.isTerm then markFree(sym, enclosure)
        def captureImplicitThis(x: Type): Unit = x match
          case tr@TermRef(x, _) if !tr.termSymbol.isStatic => captureImplicitThis(x)
          case x: ThisType if !x.tref.typeSymbol.isStaticOwner => narrowTo(x.tref.typeSymbol.asClass)
          case _ =>
        captureImplicitThis(tree.tpe)
      case tree: Select =>
        if isExpr(sym) && isLocal(sym) then markCalled(sym, enclosure)
      case tree: This =>
        narrowTo(tree.symbol.asClass)
      case tree: DefDef =>
        if sym.owner.isTerm then
          depOwner(sym) = sym.enclosingPackageClass
            // this will make methods in supercall constructors of top-level classes owned
            // by the enclosing package, which means they will be static.
            // On the other hand, all other methods will be indirectly owned by their
            // top-level class. This avoids possible deadlocks when a static method
            // has to access its enclosing object from the outside.
        else if sym.isConstructor then
          if sym.isPrimaryConstructor && isLocal(sym.owner) && !sym.owner.is(Trait) then
            // add a call edge from the constructor of a local non-trait class to
            // the class itself. This is done so that the constructor inherits
            // the free variables of the class.
            symSet(called, sym) += sym.owner
      case tree: TypeDef =>
        if sym.owner.isTerm then depOwner(sym) = sym.topLevelClass.owner
      case _ =>
  end process

  private class CollectDependencies extends TreeTraverser:
    def traverse(tree: Tree)(using Context) =
      try
        process(tree)
        traverseChildren(tree)
      catch case ex: Exception =>
        println(i"$ex while traversing $tree")
        throw ex

  /** Compute final free variables map `fvs by closing over caller dependencies. */
  private def computeFreeVars()(using Context): Unit =
    while
      changedFreeVars = false
      for
        caller <- called.keys
        callee <- called(caller)
        fvs <- free get callee
        fv <- fvs
      do
        markFree(fv, caller)
      changedFreeVars
    do ()

  /** Compute final liftedOwner map by closing over caller dependencies */
  private def computeLiftedOwners()(using Context): Unit =
    while
      changedLiftedOwner = false
      for
        caller <- called.keys
        callee <- called(caller)
      do
        val normalizedCallee = callee.skipConstructor
        val calleeOwner = normalizedCallee.owner
        if calleeOwner.isTerm then narrowLiftedOwner(caller, depOwner(normalizedCallee))
        else
          assert(calleeOwner.is(Trait))
          // methods nested inside local trait methods cannot be lifted out
          // beyond the trait. Note that we can also call a trait method through
          // a qualifier; in that case no restriction to lifted owner arises.
          if caller.isContainedIn(calleeOwner) then
            narrowLiftedOwner(caller, calleeOwner)
      changedLiftedOwner
    do ()

  inContext(rootContext) {
    CollectDependencies().traverse(rootContext.compilationUnit.tpdTree)
    computeFreeVars()
    computeLiftedOwners()
  }
end Dependencies
