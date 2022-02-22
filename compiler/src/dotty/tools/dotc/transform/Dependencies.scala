package dotty.tools.dotc
package transform

import core.*
import Symbols.*, Contexts.*, Types.*, Flags.*, Decorators.*
import SymUtils.*
import collection.mutable.{LinkedHashMap, TreeSet}
import annotation.constructorOnly

/** Exposes the dependencies of the `root` tree in three functions or maps:
 *  `freeVars`, `tracked`, and `logicalOwner`.
 */
abstract class Dependencies(root: ast.tpd.Tree, @constructorOnly rootContext: Context):
  import ast.tpd._

  /** The symbol is a method or a lazy val that will be mapped to a method */
  protected def isExpr(sym: Symbol)(using Context): Boolean

  /** The closest enclosing symbol in the current context for which `isExpr` is true */
  protected def enclosure(using Context): Symbol

  /** The set of free variables of a function, including free variables of its callees */
  def freeVars(sym: Symbol): collection.Set[Symbol] = free.getOrElse(sym, Set.empty)

  /** The set of functions that have free variables, i.e for which `freeVars` is non-empty */
  def tracked: Iterable[Symbol] = free.keys

  /** The outermost class that captures all free variables of a function
   *  that are captured by enclosinh classes (this means that the function could
   *  be placed in that class without having to add more environment parameters)
   */
  def logicalOwner: collection.Map[Symbol, Symbol] = logicOwner

  private type SymSet = TreeSet[Symbol]

  /** A map storing free variables of functions and classes */
  private val free: LinkedHashMap[Symbol, SymSet] = new LinkedHashMap

  /** A hashtable storing calls between functions */
  private val called = new LinkedHashMap[Symbol, SymSet]

  /** A map from local methods and classes to the owners to which they will be lifted as members.
   *  For methods and classes that do not have any dependencies this will be the enclosing package.
   *  symbols with packages as lifted owners will subsequently represented as static
   *  members of their toplevel class, unless their enclosing class was already static.
   *  Note: During tree transform (which runs at phase LambdaLift + 1), liftedOwner
   *  is also used to decide whether a method had a term owner before.
   */
  private val logicOwner = new LinkedHashMap[Symbol, Symbol]

  /** A flag to indicate whether new free variables have been found */
  private var changedFreeVars: Boolean = _

  /** A flag to indicate whether lifted owners have changed */
  private var changedLogicOwner: Boolean = _

  private val ord: Ordering[Symbol] = Ordering.by(_.id)
  private def newSymSet = TreeSet.empty[Symbol](ord)

  private def symSet(f: LinkedHashMap[Symbol, SymSet], sym: Symbol): SymSet =
    f.getOrElseUpdate(sym, newSymSet)

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
  private def narrowLogicOwner(sym: Symbol, owner: Symbol)(using Context): Unit =
    if sym.maybeOwner.isTerm
        && owner.isProperlyContainedIn(logicOwner(sym))
        && owner != sym
    then
      report.log(i"narrow lifted $sym to $owner")
      changedLogicOwner = true
      logicOwner(sym) = owner

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
  private def markFree(sym: Symbol, enclosure: Symbol)(using Context): Symbol =
    import Dependencies.NoPath
    try
      if !enclosure.exists then throw NoPath()
      if enclosure == sym.enclosure then NoSymbol
      else
        def nestedInConstructor(sym: Symbol): Boolean =
          sym.isConstructor
          || sym.isTerm && nestedInConstructor(sym.enclosure)
        report.debuglog(i"mark free: ${sym.showLocated} with owner ${sym.maybeOwner} marked free in $enclosure")
        val intermediate =
          if enclosure.is(PackageClass) then enclosure
          else if enclosure.isConstructor then markFree(sym, enclosure.owner.enclosure)
          else markFree(sym, enclosure.enclosure)
        if intermediate.exists then
          narrowLogicOwner(enclosure, intermediate)
        if !intermediate.isRealClass || nestedInConstructor(enclosure) then
          // Constructors and methods nested inside traits get the free variables
          // of the enclosing trait or class.
          // Conversely, local traits do not get free variables.
          // Methods inside constructors also don't have intermediates,
          // need to get all their free variables passed directly.
          if !enclosure.is(Trait) then
            if symSet(free, enclosure).add(sym) then
              changedFreeVars = true
              report.log(i"$sym is free in $enclosure")
        if intermediate.isRealClass then intermediate
        else if enclosure.isRealClass then enclosure
        else if intermediate.isClass then intermediate
        else if enclosure.isClass then enclosure
        else NoSymbol
    catch case ex: NoPath =>
      println(i"error lambda lifting ${ctx.compilationUnit}: $sym is not visible from $enclosure")
      throw ex

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
      narrowLogicOwner(enclMethod,
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
      case tree: MemberDef if isExpr(sym) && sym.owner.isTerm =>
        logicOwner(sym) = sym.enclosingPackageClass
          // this will make methods in supercall constructors of top-level classes owned
          // by the enclosing package, which means they will be static.
          // On the other hand, all other methods will be indirectly owned by their
          // top-level class. This avoids possible deadlocks when a static method
          // has to access its enclosing object from the outside.
      case tree: DefDef if sym.isPrimaryConstructor && isLocal(sym.owner) && !sym.owner.is(Trait) =>
        // add a call edge from the constructor of a local non-trait class to
        // the class itself. This is done so that the constructor inherits
        // the free variables of the class.
        symSet(called, sym) += sym.owner
      case tree: TypeDef =>
        if sym.owner.isTerm then logicOwner(sym) = sym.topLevelClass.owner
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
  private def computeLogicOwners()(using Context): Unit =
    while
      changedLogicOwner = false
      for
        caller <- called.keys
        callee <- called(caller)
      do
        val normalizedCallee = callee.skipConstructor
        val calleeOwner = normalizedCallee.owner
        if calleeOwner.isTerm then narrowLogicOwner(caller, logicOwner(normalizedCallee))
        else
          assert(calleeOwner.is(Trait))
          // methods nested inside local trait methods cannot be lifted out
          // beyond the trait. Note that we can also call a trait method through
          // a qualifier; in that case no restriction to lifted owner arises.
          if caller.isContainedIn(calleeOwner) then
            narrowLogicOwner(caller, calleeOwner)
      changedLogicOwner
    do ()

  // initialization
  inContext(rootContext) {
    CollectDependencies().traverse(root)
    computeFreeVars()
    computeLogicOwners()
  }
object Dependencies:
  private class NoPath extends Exception
end Dependencies
