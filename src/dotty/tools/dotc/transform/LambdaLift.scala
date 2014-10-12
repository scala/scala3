package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import core.Phases._
import ast.Trees._
import SymUtils._
import ExplicitOuter.outer
import util.Attachment
import util.NameTransformer
import util.Positions._
import collection.{ mutable, immutable }
import collection.mutable.{ HashMap, HashSet, LinkedHashMap, LinkedHashSet, TreeSet }

object LambdaLift {
  val NJ = NameTransformer.NAME_JOIN_STRING
}

class LambdaLift extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import LambdaLift._
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "lambdalift"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Constructors])
    // Constructors has to happen before LambdaLift because the lambda lift logic
    // becomes simpler if it can assume that parameter accessors have already been
    // converted to parameters in super calls. Without this it is very hard to get
    // lambda lift for super calls right. Witness the implementation restrictions to
    // this effect in scalac.

  override def treeTransformPhase = thisTransform.next
  override def relaxedTyping = true

  private type SymSet = TreeSet[Symbol]

  /** A map storing free variables of functions and classes */
  private val free = new LinkedHashMap[Symbol, SymSet]

  /** A map storing the free variable proxies of functions and classes.
   *  For every function and class, this is a map from the free variables
   *  of that function or class to the proxy symbols accessing them.
   */
  private val proxyMap = new LinkedHashMap[Symbol, Map[Symbol, Symbol]]

  /** A hashtable storing calls between functions */
  private val called = new LinkedHashMap[Symbol, SymSet]

  /** Symbols that are called from an inner class. */
  private val calledFromInner = new HashSet[Symbol]

  /** A map from local methods and classes to the owners to which they will be lifted as members.
   *  For methods and classes that do not have any dependencies this will be the enclosing package.
   *  symbols with packages as lifted owners will subsequently represented as static
   *  members of their toplevel class.
   */
  private val liftedOwner = new HashMap[Symbol, Symbol]

  /** Buffers for lifted out classes and methods, indexed by owner */
  private val liftedDefs = new HashMap[Symbol, mutable.ListBuffer[Tree]]

  /** A flag to indicate whether new free variables have been found */
  private var changedFreeVars: Boolean = _

  /** A flag to indicate whether lifted owners have changed */
  private var changedLiftedOwner: Boolean = _

  private val ord: Ordering[Symbol] = Ordering.by((_: Symbol).id) // Dotty deviation: Type annotation needed. TODO: figure out why
  private def newSymSet = TreeSet.empty[Symbol](ord)

  private def symSet(f: LinkedHashMap[Symbol, SymSet], sym: Symbol): SymSet =
    f.getOrElseUpdate(sym, newSymSet)

  def proxies(sym: Symbol): List[Symbol] = {
    val pm: Map[Symbol, Symbol] = proxyMap.getOrElse(sym, Map.empty) // Dotty deviation: Type annotation needed. TODO: figure out why
    free.getOrElse(sym, Nil).toList.map(pm)
  }

  def narrowLiftedOwner(sym: Symbol, owner: Symbol)(implicit ctx: Context) = {
    println(i"narrow lifted $sym")
    if (sym.owner.skipConstructor.isTerm &&
        owner.isProperlyContainedIn(liftedOwner(sym))) {
      changedLiftedOwner = true
      liftedOwner(sym) = owner
    }
  }

  /** Mark symbol `sym` as being free in `enclosure`, unless `sym`
   *  is defined in `enclosure` or there is a class between `enclosure`s owner
   *  and the owner of `sym`.
   *  Return `true` if there is no class between `enclosure` and
   *  the owner of sym.
   *  pre: sym.owner.isTerm, (enclosure.isMethod || enclosure.isClass)
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
  private def markFree(sym: Symbol, enclosure: Symbol)(implicit ctx: Context): Boolean = {
    println(i"mark free: ${sym.showLocated} with owner ${sym.maybeOwner} marked free in $enclosure")
    (enclosure == sym.enclosure) || {
      ctx.debuglog(i"$enclosure != ${sym.enclosure}")
      narrowLiftedOwner(enclosure, sym.enclosingClass)
      if (enclosure.is(PackageClass) ||
          !markFree(sym, enclosure.skipConstructor.enclosure)) false
      else {
        val ss = symSet(free, enclosure)
        if (!ss(sym)) {
          ss += sym
          changedFreeVars = true
          ctx.debuglog(i"$sym is free in $enclosure")
        }
        !enclosure.isClass
      }
    }
  }

  private def markCalled(callee: Symbol, caller: Symbol)(implicit ctx: Context): Unit = {
    ctx.debuglog(i"mark called: $callee of ${callee.owner} is called by $caller")
    assert(callee.skipConstructor.owner.isTerm)
    symSet(called, caller) += callee
    if (callee.enclosingClass != caller.enclosingClass) calledFromInner += callee
  }

  private class CollectDependencies(implicit ctx: Context) extends EnclosingMethodTraverser {
    def traverse(enclMeth: Symbol, tree: Tree) = try { //debug
      val enclosure = enclMeth.skipConstructor
      val sym = tree.symbol
      tree match {
        case tree: Ident =>
          if (sym.maybeOwner.isTerm)
            if (sym is (Method, butNot = Label)) markCalled(sym, enclosure)
            else if (sym.isTerm) markFree(sym, enclosure)
        case tree: Select =>
          if (sym.isConstructor && sym.owner.owner.isTerm)
            markCalled(sym, enclosure)
        case tree: This =>
          val thisClass = tree.symbol.asClass
          val enclClass = enclosure.enclosingClass
          if (!thisClass.isStaticOwner && thisClass != enclClass)
            narrowLiftedOwner(enclosure,
               if (enclClass.isContainedIn(thisClass)) thisClass
               else enclClass) // unknown this reference, play it safe and assume the narrowest possible owner
        case tree: DefDef =>
          if (sym.owner.isTerm && !sym.is(Label)) liftedOwner(sym) = sym.topLevelClass.owner
          else if (sym.isPrimaryConstructor && sym.owner.owner.isTerm) symSet(called, sym) += sym.owner
        case tree: TypeDef =>
          if (sym.owner.isTerm) liftedOwner(sym) = sym.topLevelClass.owner
        case tree: Template =>
          liftedDefs(enclosure) = new mutable.ListBuffer
        case _ =>
      }
      foldOver(enclosure, tree)
    } catch { //debug
      case ex: Exception =>
        println(i"$ex while traversing $tree")
        throw ex
    }
  }

  /** Compute final free variables map `fvs by closing over caller dependencies. */
  private def computeFreeVars()(implicit ctx: Context): Unit =
    do {
      changedFreeVars = false
      // println(s"called = ${called.toList map { case (from, to) => from.showLocated + " -> " + to.toList.map(_.showLocated) }}")
      for {
        caller <- called.keys
        callee <- called(caller)
        fvs <- free get callee
        fv <- fvs
      } markFree(fv, caller)
    } while (changedFreeVars)

  /** Compute final liftedOwner map by closing over caller dependencies */
  private def computeLiftedOwners()(implicit ctx: Context): Unit =
    do {
      changedLiftedOwner = false
      for {
        caller <- called.keys
        callee <- called(caller)
      } narrowLiftedOwner(caller, liftedOwner(callee.skipConstructor))
    } while (changedLiftedOwner)

  private def newName(sym: Symbol)(implicit ctx: Context): Name = {
    def freshen(prefix: String): Name = {
      val fname = ctx.freshName(prefix)
      if (sym.isType) fname.toTypeName else fname.toTermName
    }
    if (sym.isAnonymousFunction && sym.owner.is(Method, butNot = Label))
      freshen(sym.name.toString ++ NJ ++ sym.owner.name ++ NJ)
    else if (sym is ModuleClass)
      freshen(sym.sourceModule.name.toString ++ NJ).moduleClassName
    else
      freshen(sym.name.toString ++ NJ)
  }

  private def generateProxies()(implicit ctx: Context): Unit =
    for ((owner, freeValues) <- free.toIterator) {
      val newFlags = Synthetic | (if (owner.isClass) ParamAccessor | Private else Param)
      ctx.debuglog(i"free var proxy: ${owner.showLocated}, ${freeValues.toList}%, %")
      proxyMap(owner) = {
        for (fv <- freeValues.toList) yield {
          val proxyName = newName(fv)
          val proxy = ctx.newSymbol(owner, proxyName.asTermName, newFlags, fv.info, coord = fv.coord)
          if (owner.isClass) proxy.enteredAfter(thisTransform)
          (fv, proxy)
        }
      }.toMap
    }

  private def liftedInfo(local: Symbol)(implicit ctx: Context): Type = local.info match {
    case mt @ MethodType(pnames, ptypes) =>
      val ps = proxies(local.skipConstructor)
      MethodType(
        pnames ++ ps.map(_.name.asTermName),
        ptypes ++ ps.map(_.info),
        mt.resultType)
    case info => info
  }

  private def liftLocals()(implicit ctx: Context): Unit = {
    for ((local, lOwner) <- liftedOwner) {
      val (newOwner, maybeStatic) =
        if (lOwner is Package) (local.topLevelClass, Static)
        else (lOwner, EmptyFlags)
      val maybeNotJavaPrivate = if (calledFromInner(local)) NotJavaPrivate else EmptyFlags
      local.copySymDenotation(
        owner = newOwner,
        name = newName(local),
        initFlags = local.flags | Private | maybeStatic | maybeNotJavaPrivate,
        info = liftedInfo(local)).installAfter(thisTransform)
      if (local.isClass)
        for (member <- local.asClass.decls)
          if (member.isConstructor) {
            val linfo = liftedInfo(member)
            if (linfo ne member.info)
              member.copySymDenotation(info = linfo).installAfter(thisTransform)
          }
    }
  }

  override def init(implicit ctx: Context, info: TransformerInfo) = {
    assert(ctx.phase == thisTransform)
    (new CollectDependencies).traverse(NoSymbol, ctx.compilationUnit.tpdTree)
    computeFreeVars()
    computeLiftedOwners()
    generateProxies()(ctx.withPhase(thisTransform.next))
    liftLocals()(ctx.withPhase(thisTransform.next))
  }

  private def currentEnclosure(implicit ctx: Context) =
    ctx.owner.enclosingMethod.skipConstructor

  private def inCurrentOwner(sym: Symbol)(implicit ctx: Context) =
    sym.enclosure == currentEnclosure

  private def proxy(sym: Symbol)(implicit ctx: Context): Symbol = {
    def searchIn(enclosure: Symbol): Symbol = {
      if (!enclosure.exists)
        throw new IllegalArgumentException(i"Could not find proxy for ${sym.showDcl} in ${sym.ownersIterator.toList}, currentOwner= $currentEnclosure")
      ctx.debuglog(i"searching for $sym(${sym.owner}) in $enclosure")
      proxyMap get enclosure match {
        case Some(pmap) =>
          pmap get sym match {
            case Some(proxy) => return proxy
            case none =>
          }
        case none =>
      }
      searchIn(enclosure.enclosure)
    }
    if (inCurrentOwner(sym)) sym else searchIn(currentEnclosure)
  }

  private def memberRef(sym: Symbol)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val clazz = sym.owner
    val qual = if (clazz.isStaticOwner) singleton(clazz.thisType) else outer.path(clazz)
    transformFollowingDeep(qual.select(sym))
  }

  private def proxyRef(sym: Symbol)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val psym = proxy(sym)
    transformFollowingDeep(if (psym.owner.isTerm) ref(psym) else memberRef(psym))
  }

  private def addFreeArgs(sym: Symbol, args: List[Tree])(implicit ctx: Context, info: TransformerInfo) =
    free get sym match {
      case Some(fvs) => args ++ fvs.toList.map(proxyRef(_))
      case _         => args
    }

  private def addFreeParams(tree: Tree, proxies: List[Symbol])(implicit ctx: Context, info: TransformerInfo): Tree = proxies match {
    case Nil => tree
    case proxies =>
      val ownProxies =
        if (!tree.symbol.isConstructor) proxies
        else proxies.map(_.copy(owner = tree.symbol, flags = Synthetic | Param))
      val freeParamDefs = ownProxies.map(proxy =>
        transformFollowingDeep(ValDef(proxy.asTerm).withPos(tree.pos)).asInstanceOf[ValDef])
      tree match {
        case tree: DefDef =>
          cpy.DefDef(tree)(vparamss = tree.vparamss.map(_ ++ freeParamDefs))
        case tree: Template =>
          cpy.Template(tree)(body = tree.body ++ freeParamDefs)
      }
  }

  private def liftDef(tree: MemberDef)(implicit ctx: Context): Tree = {
    liftedDefs(tree.symbol.owner) += rename(tree, tree.symbol.name)
    EmptyTree
  }

  private def needsLifting(sym: Symbol) = liftedOwner contains sym

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = {
    val sym = tree.symbol
    tree.tpe match {
      case TermRef(NoPrefix, _) if sym.enclosure != currentEnclosure && !sym.isStatic =>
        (if (sym is Method) memberRef(sym) else proxyRef(sym)).withPos(tree.pos)
      case _ =>
        tree
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
    cpy.Apply(tree)(tree.fun, addFreeArgs(tree.symbol.skipConstructor, tree.args)).withPos(tree.pos)

  override def transformClosure(tree: Closure)(implicit ctx: Context, info: TransformerInfo) =
    cpy.Closure(tree)(env = addFreeArgs(tree.meth.symbol, tree.env))

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo) = {
    val sym = tree.symbol
    val proxyHolder = sym.skipConstructor
    if (needsLifting(proxyHolder)) {
      val paramsAdded = addFreeParams(tree, proxies(proxyHolder)).asInstanceOf[DefDef]
      if (sym.isConstructor) paramsAdded else liftDef(paramsAdded)
    }
    else tree
  }

  override def transformReturn(tree: Return)(implicit ctx: Context, info: TransformerInfo) = tree.expr match {
    case Block(stats, value) =>
      Block(stats, Return(value, tree.from)).withPos(tree.pos)
    case _ =>
      tree
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = ctx.owner
    val impl = addFreeParams(tree, proxies(cls)).asInstanceOf[Template]
    cpy.Template(impl)(body = impl.body ++ liftedDefs.remove(cls).get)
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo) =
    if (needsLifting(tree.symbol)) liftDef(tree) else tree
  }


/* done in lazyvals?
        case Block(stats, expr0) =>
          val (lzyVals, rest) = stats partition {
            case stat: ValDef => stat.symbol.isLazy || stat.symbol.isModuleVar
            case _            => false
          }
          if (lzyVals.isEmpty) tree
          else treeCopy.Block(tree, lzyVals ::: rest, expr0)

*/
