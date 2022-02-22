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
import SymUtils._
import ExplicitOuter.outer
import util.Store
import collection.mutable.{HashMap, LinkedHashMap, ListBuffer}

object LambdaLift:
  import ast.tpd._

  val name: String = "lambdaLift"
  val description: String = "lifts out nested functions to class scope"

  /** The core lambda lift functionality. */
  class Lifter(thisPhase: MiniPhase & DenotTransformer)(using Context):

    /** The outer parameter of a constructor */
    private val outerParam = new HashMap[Symbol, Symbol]

    /** Buffers for lifted out classes and methods, indexed by owner */
    val liftedDefs: HashMap[Symbol, ListBuffer[Tree]] = new HashMap

    val deps = new Dependencies(ctx.compilationUnit.tpdTree, ctx.withPhase(thisPhase)):
      def isExpr(sym: Symbol)(using Context): Boolean = sym.is(Method)
      def enclosure(using Context) = ctx.owner.enclosingMethod

      override def process(tree: Tree)(using Context): Unit =
        super.process(tree)
        tree match
          case tree: DefDef if tree.symbol.isConstructor =>
            tree.termParamss.head.find(_.name == nme.OUTER) match
              case Some(vdef) => outerParam(tree.symbol) = vdef.symbol
              case _ =>
          case tree: Template =>
            liftedDefs(tree.symbol.owner) = new ListBuffer
          case _ =>
    end deps

    /** A map storing the free variable proxies of functions and classes.
     *  For every function and class, this is a map from the free variables
     *  of that function or class to the proxy symbols accessing them.
     */
    private val proxyMap = new LinkedHashMap[Symbol, Map[Symbol, Symbol]]

    def proxyOf(sym: Symbol, fv: Symbol): Symbol = proxyMap.getOrElse(sym, Map.empty)(fv)

    def proxies(sym: Symbol): List[Symbol] =
      deps.freeVars(sym).toList.map(proxyOf(sym, _))

    private def newName(sym: Symbol)(using Context): Name =
      if (sym.isAnonymousFunction && sym.owner.is(Method))
        sym.name.replace {
          case name: SimpleName => ExpandPrefixName(sym.owner.name.asTermName, name)
        }.freshened
      else sym.name.freshened

    private def generateProxies()(using Context): Unit =
      for owner <- deps.tracked do
        val fvs = deps.freeVars(owner).toList
        val newFlags = Synthetic | (if (owner.isClass) ParamAccessor | Private else Param)
        report.debuglog(i"free var proxy of ${owner.showLocated}: $fvs%, %")
        val freeProxyPairs =
          for fv <- fvs yield
            val proxyName = newName(fv)
            val proxy =
              newSymbol(owner, proxyName.asTermName, newFlags, fv.info, coord = fv.coord)
                .enteredAfter(thisPhase)
            (fv, proxy)
        proxyMap(owner) = freeProxyPairs.toMap

    private def liftedInfo(local: Symbol)(using Context): Type = local.info match {
      case MethodTpe(pnames, ptypes, restpe) =>
        val ps = proxies(local)
        MethodType(
          ps.map(_.name.asTermName) ++ pnames,
          ps.map(_.info) ++ ptypes,
          restpe)
      case info => info
    }

    private def liftLocals()(using Context): Unit = {
      for ((local, lOwner) <- deps.logicalOwner) {
        val (newOwner, maybeStatic) =
          if (lOwner is Package) {
            val encClass = local.enclosingClass
            val topClass = local.topLevelClass
            val preferEncClass =
              encClass.isStatic &&
                // non-static classes can capture owners, so should be avoided
              (encClass.isProperlyContainedIn(topClass) ||
                // can be false for symbols which are defined in some weird combination of supercalls.
               encClass.is(ModuleClass, butNot = Package)
                // needed to not cause deadlocks in classloader. see t5375.scala
              )
            if (preferEncClass) (encClass, EmptyFlags)
            else (topClass, JavaStatic)
          }
          else (lOwner, EmptyFlags)
        // Drop Module because class is no longer a singleton in the lifted context.
        var initFlags = local.flags &~ Module | Private | Lifted | maybeStatic
        if (local is Method)
          if (newOwner is Trait)
            // Drop Final when a method is lifted into a trait.
            // According to the JVM specification, a method declared inside interface cannot have the final flag.
            // "Methods of interfaces may have any of the flags in Table 4.6-A set except ACC_PROTECTED, ACC_FINAL, ..."
            // (https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6)
            initFlags = initFlags &~ Final
          else
            // Add Final when a method is lifted into a class.
            initFlags = initFlags | Final
        local.copySymDenotation(
          owner = newOwner,
          name = newName(local),
          initFlags = initFlags,
          info = liftedInfo(local)).installAfter(thisPhase)
      }
      for (local <- deps.tracked)
        if (!deps.logicalOwner.contains(local))
          local.copySymDenotation(info = liftedInfo(local)).installAfter(thisPhase)
    }

    def currentEnclosure(using Context): Symbol =
      ctx.owner.enclosingMethodOrClass

    private def inCurrentOwner(sym: Symbol)(using Context) =
      sym.enclosure == currentEnclosure

    private def proxy(sym: Symbol)(using Context): Symbol = {
      def liftedEnclosure(sym: Symbol) =
        deps.logicalOwner.getOrElse(sym, sym.enclosure)
      def searchIn(enclosure: Symbol): Symbol = {
        if (!enclosure.exists) {
          def enclosures(encl: Symbol): List[Symbol] =
            if (encl.exists) encl :: enclosures(liftedEnclosure(encl)) else Nil
          throw new IllegalArgumentException(i"Could not find proxy for ${sym.showDcl} in ${sym.ownersIterator.toList}, encl = $currentEnclosure, owners = ${currentEnclosure.ownersIterator.toList}%, %; enclosures = ${enclosures(currentEnclosure)}%, %")
        }
        report.debuglog(i"searching for $sym(${sym.owner}) in $enclosure")
        proxyMap get enclosure match {
          case Some(pmap) =>
            pmap get sym match {
              case Some(proxy) => return proxy
              case none =>
            }
          case none =>
        }
        searchIn(liftedEnclosure(enclosure))
      }
      if (inCurrentOwner(sym)) sym else searchIn(currentEnclosure)
    }

    def memberRef(sym: Symbol)(using Context): Tree = {
      val clazz = sym.enclosingClass
      val qual =
        if (clazz.isStaticOwner || ctx.owner.enclosingClass == clazz)
          singleton(clazz.thisType)
        else if (ctx.owner.isConstructor)
          outerParam.get(ctx.owner) match {
            case Some(param) => outer.path(start = Ident(param.termRef), toCls = clazz)
            case _ => outer.path(toCls = clazz)
          }
        else outer.path(toCls = clazz)
      thisPhase.transformFollowingDeep(qual.select(sym))
    }

    def proxyRef(sym: Symbol)(using Context): Tree = {
      val psym = atPhase(thisPhase)(proxy(sym))
      thisPhase.transformFollowingDeep(if (psym.owner.isTerm) ref(psym) else memberRef(psym))
    }

    def addFreeArgs(sym: Symbol, args: List[Tree])(using Context): List[Tree] =
      val fvs = deps.freeVars(sym)
      if fvs.nonEmpty then fvs.toList.map(proxyRef(_)) ++ args else args

    def addFreeParams(tree: Tree, proxies: List[Symbol])(using Context): Tree = proxies match {
      case Nil => tree
      case proxies =>
        val sym = tree.symbol
        val freeParamDefs = proxies.map(proxy =>
          thisPhase.transformFollowingDeep(ValDef(proxy.asTerm).withSpan(tree.span)).asInstanceOf[ValDef])
        def proxyInit(field: Symbol, param: Symbol) =
          thisPhase.transformFollowingDeep(memberRef(field).becomes(ref(param)))

        /** Initialize proxy fields from proxy parameters and map `rhs` from fields to parameters */
        def copyParams(rhs: Tree) = {
          val fvs = deps.freeVars(sym.owner).toList
          val classProxies = fvs.map(proxyOf(sym.owner, _))
          val constrProxies = fvs.map(proxyOf(sym, _))
          report.debuglog(i"copy params ${constrProxies.map(_.showLocated)}%, % to ${classProxies.map(_.showLocated)}%, %}")
          seq(classProxies.lazyZip(constrProxies).map(proxyInit), rhs)
        }

        tree match {
          case tree: DefDef =>
            cpy.DefDef(tree)(
                paramss = tree.termParamss.map(freeParamDefs ++ _),
                rhs =
                  if (sym.isPrimaryConstructor && !sym.owner.is(Trait)) copyParams(tree.rhs)
                  else tree.rhs)
          case tree: Template =>
            cpy.Template(tree)(body = freeParamDefs ++ tree.body)
        }
    }

    def liftDef(tree: MemberDef)(using Context): Tree = {
      val buf = liftedDefs(tree.symbol.owner)
      thisPhase.transformFollowing(rename(tree, tree.symbol.name)).foreachInThicket(buf += _)
      EmptyTree
    }

    def needsLifting(sym: Symbol): Boolean = deps.logicalOwner.contains(sym)

    // initialization
    atPhase(thisPhase.next) {
      generateProxies()
      liftLocals()
    }
  end Lifter
end LambdaLift

/** This phase performs the necessary rewritings to eliminate classes and methods
 *  nested in other methods. In detail:
 *   1. It adds all free variables of local functions as additional parameters (proxies).
 *   2. It rebinds references to free variables to the corresponding proxies,
 *   3. It lifts all local functions and classes out as far as possible, but at least
 *      to the enclosing class.
 *   4. It stores free variables of non-trait classes as additional fields of the class.
 *      The fields serve as proxies for methods in the class, which avoids the need
 *      of passing additional parameters to these methods.
 *
 *  A particularly tricky case are local traits. These cannot store free variables
 *  as field proxies, because LambdaLift runs after Mixin, so the fields cannot be
 *  expanded anymore. Instead, methods of local traits get free variables of
 *  the trait as additional proxy parameters. The difference between local classes
 *  and local traits is illustrated by the two rewritings below.
 *
 *     def f(x: Int) = {           def f(x: Int) = new C(x).f2
 *       class C {          ==>    class C(x$1: Int) {
 *         def f2 = x                def f2 = x$1
 *       }                         }
 *       new C().f2
 *     }
 *
 *     def f(x: Int) = {           def f(x: Int) = new C().f2(x)
 *       trait T {          ==>    trait T
 *         def f2 = x                def f2(x$1: Int) = x$1
 *       }                         }
 *       class C extends T         class C extends T
 *       new C().f2
 *     }
 */
class LambdaLift extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import LambdaLift._
  import ast.tpd._

  override def phaseName: String = LambdaLift.name

  override def description: String = LambdaLift.description

  override def relaxedTypingInGroup: Boolean = true
    // Because it adds free vars as additional proxy parameters

  override def runsAfterGroupsOf: Set[String] = Set(Constructors.name, HoistSuperArgs.name)
    // Constructors has to happen before LambdaLift because the lambda lift logic
    // becomes simpler if it can assume that parameter accessors have already been
    // converted to parameters in super calls. Without this it is very hard to get
    // lambda lift for super calls right. Witness the implementation restrictions to
    // this effect in scalac.

  private var Lifter: Store.Location[Lifter] = _
  private def lifter(using Context) = ctx.store(Lifter)

  override def initContext(ctx: FreshContext): Unit =
    Lifter = ctx.addLocation[Lifter]()

  override def prepareForUnit(tree: Tree)(using Context): Context =
    ctx.fresh.updateStore(Lifter, new Lifter(thisPhase))

  override def transformIdent(tree: Ident)(using Context): Tree = {
    val sym = tree.symbol
    tree.tpe match {
      case tpe @ TermRef(prefix, _) =>
        val lft = lifter
        if (prefix eq NoPrefix)
          if (sym.enclosure != lft.currentEnclosure && !sym.isStatic)
            (if (sym is Method) lft.memberRef(sym) else lft.proxyRef(sym)).withSpan(tree.span)
          else if (sym.owner.isClass) // sym was lifted out
            ref(sym).withSpan(tree.span)
          else
            tree
        else if (!prefixIsElidable(tpe)) ref(tpe)
        else tree
      case _ =>
        tree
    }
  }

  override def transformSelect(tree: Select)(using Context): Tree =
    val denot = tree.denot
    val sym = tree.symbol
    // The Lifter updates the type of symbols using `installAfter` to give them a
    // new `SymDenotation`, but that doesn't affect non-sym denotations, so we
    // reload them manually here.
    // Note: If you tweak this code, make sure to test your changes with
    // `Config.reuseSymDenotations` set to false to exercise this path more.
    if denot.isInstanceOf[NonSymSingleDenotation] && lifter.deps.freeVars(sym).nonEmpty then
      tree.qualifier.select(sym).withSpan(tree.span)
    else tree

  override def transformApply(tree: Apply)(using Context): Apply =
    cpy.Apply(tree)(tree.fun, lifter.addFreeArgs(tree.symbol, tree.args)).withSpan(tree.span)

  override def transformClosure(tree: Closure)(using Context): Closure =
    cpy.Closure(tree)(env = lifter.addFreeArgs(tree.meth.symbol, tree.env))

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val sym = tree.symbol
    val lft = lifter
    val paramsAdded =
      if lft.deps.freeVars(sym).nonEmpty then lft.addFreeParams(tree, lft.proxies(sym)).asInstanceOf[DefDef]
      else tree
    if (lft.needsLifting(sym)) lft.liftDef(paramsAdded)
    else paramsAdded
  }

  override def transformReturn(tree: Return)(using Context): Tree = tree.expr match {
    case Block(stats, value) =>
      Block(stats, Return(value, tree.from)).withSpan(tree.span)
    case _ =>
      tree
  }

  override def transformTemplate(tree: Template)(using Context): Template = {
    val cls = ctx.owner
    val lft = lifter
    val impl = lft.addFreeParams(tree, lft.proxies(cls)).asInstanceOf[Template]
    cpy.Template(impl)(body = impl.body ++ lft.liftedDefs.remove(cls).get)
  }

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    if (lifter.needsLifting(tree.symbol)) lifter.liftDef(tree) else tree
}
