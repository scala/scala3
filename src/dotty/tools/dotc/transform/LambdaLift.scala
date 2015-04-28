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
  private val NJ = NameTransformer.NAME_JOIN_STRING
  private class NoPath extends Exception
}

class LambdaLift extends MiniPhase with IdentityDenotTransformer { thisTransform =>
  import LambdaLift._
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "lambdaLift"
  val treeTransform = new LambdaLifter

  override def relaxedTyping = true

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Constructors])
    // Constructors has to happen before LambdaLift because the lambda lift logic
    // becomes simpler if it can assume that parameter accessors have already been
    // converted to parameters in super calls. Without this it is very hard to get
    // lambda lift for super calls right. Witness the implementation restrictions to
    // this effect in scalac.

  class LambdaLifter extends TreeTransform {
    override def phase = thisTransform

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

    /** Set `liftedOwner(sym)` to `owner` if `owner` is more deeply nested
     *  than the previous value of `liftedowner(sym)`.
     */
    def narrowLiftedOwner(sym: Symbol, owner: Symbol)(implicit ctx: Context) = {
      if (sym.owner.isTerm &&
        owner.isProperlyContainedIn(liftedOwner(sym)) &&
        owner != sym) {
        ctx.log(i"narrow lifted $sym to $owner")
        changedLiftedOwner = true
        liftedOwner(sym) = owner
      }
    }

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
     *  Return the closest enclosing intermediate class between `enclosure` and
     *  the owner of sym, or NoSymbol if none exists.
     *
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
    private def markFree(sym: Symbol, enclosure: Symbol)(implicit ctx: Context): Symbol = try {
      if (!enclosure.exists) throw new NoPath
      if (enclosure == sym.enclosure) NoSymbol
      else {
        ctx.log(i"mark free: ${sym.showLocated} with owner ${sym.maybeOwner} marked free in $enclosure")
        ctx.debuglog(i"$enclosure != ${sym.enclosure}")
        val intermediate =
          if (enclosure.is(PackageClass)) enclosure
          else markFree(sym, enclosure.skipConstructor.enclosure)
            // `enclosure` might be a constructor, in which case we want the enclosure
            // of the enclosing class, so skipConstructor is needed here.
        if (intermediate.exists) {
          narrowLiftedOwner(enclosure, intermediate)
          intermediate
        }
        else {
          narrowLiftedOwner(enclosure, sym.enclosingClass)
          val ss = symSet(free, enclosure)
          if (!ss(sym)) {
            ss += sym
            changedFreeVars = true
            ctx.debuglog(i"$sym is free in $enclosure")
          }
          if (enclosure.isClass) enclosure else NoSymbol
        }
      }
    } catch {
      case ex: NoPath =>
        println(i"error lambda lifting ${ctx.compilationUnit}: $sym is not visible from $enclosure")
        throw ex
    }

    private def markCalled(callee: Symbol, caller: Symbol)(implicit ctx: Context): Unit = {
      ctx.debuglog(i"mark called: $callee of ${callee.owner} is called by $caller")
      assert(callee.skipConstructor.owner.isTerm)
      symSet(called, caller) += callee
      if (callee.enclosingClass != caller.enclosingClass) calledFromInner += callee
    }

    private class CollectDependencies extends EnclosingMethodTraverser {
      def traverse(enclMeth: Symbol, tree: Tree)(implicit ctx: Context) = try { //debug
        val enclosure = enclMeth.skipConstructor
        val sym = tree.symbol
        def narrowTo(thisClass: ClassSymbol) = {
          val enclClass = enclosure.enclosingClass
          if (!thisClass.isStaticOwner)
            narrowLiftedOwner(enclosure,
              if (enclClass.isContainedIn(thisClass)) thisClass
              else enclClass) // unknown this reference, play it safe and assume the narrowest possible owner
        }
        tree match {
          case tree: Ident =>
            if (sym.maybeOwner.isTerm) {
              if (sym is Label)
                assert(enclosure == sym.enclosure,
                  i"attempt to refer to label $sym from nested $enclosure")
              else if (sym is Method) markCalled(sym, enclosure)
              else if (sym.isTerm) markFree(sym, enclosure)
            } else if (sym.maybeOwner.isClass)
              narrowTo(sym.owner.asClass)
          case tree: Select =>
            if (sym.isConstructor && sym.owner.owner.isTerm)
              markCalled(sym, enclosure)
          case tree: This =>
            narrowTo(tree.symbol.asClass)
          case tree: DefDef =>
            if (sym.owner.isTerm && !sym.is(Label)) liftedOwner(sym) = sym.topLevelClass.owner
            else if (sym.isPrimaryConstructor && sym.owner.owner.isTerm) symSet(called, sym) += sym.owner
          case tree: TypeDef =>
            if (sym.owner.isTerm) liftedOwner(sym) = sym.topLevelClass.owner
          case tree: Template =>
            liftedDefs(tree.symbol.owner) = new mutable.ListBuffer
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
          ps.map(_.name.asTermName) ++ pnames,
          ps.map(_.info) ++ ptypes,
          mt.resultType)
      case info => info
    }

    private def liftLocals()(implicit ctx: Context): Unit = {
      for ((local, lOwner) <- liftedOwner) {
        val (newOwner, maybeStatic) =
          if (lOwner is Package) (local.topLevelClass, JavaStatic)
          else (lOwner, EmptyFlags)
        val maybeNotJavaPrivate = if (calledFromInner(local)) NotJavaPrivate else EmptyFlags
        local.copySymDenotation(
          owner = newOwner,
          name = newName(local),
          initFlags = local.flags &~ InSuperCall | Private | maybeStatic | maybeNotJavaPrivate,
          info = liftedInfo(local)).installAfter(thisTransform)
        if (local.isClass)
          for (member <- local.asClass.info.decls)
            if (member.isConstructor) {
              val linfo = liftedInfo(member)
              if (linfo ne member.info)
                member.copySymDenotation(info = linfo).installAfter(thisTransform)
            }
      }
    }

    private def init(implicit ctx: Context) = {
      (new CollectDependencies).traverse(NoSymbol, ctx.compilationUnit.tpdTree)
      computeFreeVars()
      computeLiftedOwners()
      generateProxies()(ctx.withPhase(thisTransform.next))
      liftLocals()(ctx.withPhase(thisTransform.next))
    }

    override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
      val lifter = new LambdaLifter
      lifter.init(ctx.withPhase(thisTransform))
      lifter
    }

    private def currentEnclosure(implicit ctx: Context) =
      ctx.owner.enclosingMethod.skipConstructor

    private def inCurrentOwner(sym: Symbol)(implicit ctx: Context) =
      sym.enclosure == currentEnclosure

    private def proxy(sym: Symbol)(implicit ctx: Context): Symbol = {
      def searchIn(enclosure: Symbol): Symbol = {
        if (!enclosure.exists) {
          def enclosures(encl: Symbol): List[Symbol] =
            if (encl.exists) encl :: enclosures(encl.enclosure) else Nil
          throw new IllegalArgumentException(i"Could not find proxy for ${sym.showDcl} in ${sym.ownersIterator.toList}, encl = $currentEnclosure, owners = ${currentEnclosure.ownersIterator.toList}%, %; enclosures = ${enclosures(currentEnclosure)}%, %")
        }
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
      val clazz = sym.enclosingClass
      val qual =
        if (clazz.isStaticOwner) singleton(clazz.thisType)
        else outer(ctx.withPhase(thisTransform)).path(clazz)
      transformFollowingDeep(qual.select(sym))
    }

    private def proxyRef(sym: Symbol)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val psym = proxy(sym)(ctx.withPhase(thisTransform))
      transformFollowingDeep(if (psym.owner.isTerm) ref(psym) else memberRef(psym))
    }

    private def addFreeArgs(sym: Symbol, args: List[Tree])(implicit ctx: Context, info: TransformerInfo) =
      free get sym match {
        case Some(fvs) => fvs.toList.map(proxyRef(_)) ++ args
        case _ => args
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
            cpy.DefDef(tree)(vparamss = tree.vparamss.map(freeParamDefs ++ _))
          case tree: Template =>
            cpy.Template(tree)(body = freeParamDefs ++ tree.body)
        }
    }

    private def liftDef(tree: MemberDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val buf = liftedDefs(tree.symbol.owner)
      transformFollowing(rename(tree, tree.symbol.name)).foreachInThicket(buf += _)
      EmptyTree
    }

    private def needsLifting(sym: Symbol) = liftedOwner contains sym

    override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = {
      val sym = tree.symbol
      tree.tpe match {
        case tpe @ TermRef(prefix, _) =>
          if (prefix eq NoPrefix)
            if (sym.enclosure != currentEnclosure && !sym.isStatic)
              (if (sym is Method) memberRef(sym) else proxyRef(sym)).withPos(tree.pos)
            else if (sym.owner.isClass) // sym was lifted out
              ref(sym).withPos(tree.pos)
            else
              tree
          else if (!prefixIsElidable(tpe)) ref(tpe)
          else tree
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
}
