package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.NamedArg
import dotty.tools.dotc.ast.{Trees, untpd, tpd, TreeTypeMap}
import Trees._
import core._
import Flags._
import Symbols._
import Types._
import Decorators._
import Constants._
import StdNames.nme
import Contexts.Context
import Names.{Name, TermName, EmptyTermName}
import NameOps._
import NameKinds.{ClassifiedNameKind, InlineAccessorName, UniqueInlineName}
import ProtoTypes.selectionProto
import SymDenotations.SymDenotation
import Annotations._
import transform.{ExplicitOuter, AccessProxies}
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import collection.mutable
import transform.TypeUtils._
import reporting.trace
import util.Positions.Position
import util.Property
import ast.TreeInfo

object Inliner {
  import tpd._

  /** A key to be used in a context property that provides a map from enclosing implicit
   *  value bindings to their right hand sides.
   */
  private val InlineBindings = new Property.Key[MutableSymbolMap[Tree]]

  /** A map from the symbols of all enclosing inline value bindings to their right hand sides */
  def inlineBindings(implicit ctx: Context): MutableSymbolMap[Tree] =
    ctx.property(InlineBindings).get

  /** `sym` has a transparent method with a known body to inline (note: definitions coming
   *  from Scala2x class files might be `@forceInline`, but still lack that body.
   */
  def hasBodyToInline(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isTransparentMethod && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.unforcedAnnotation(defn.BodyAnnot).get.tree

  /** Should call with method `meth` be inlined in this context? */
  def isInlineable(meth: Symbol)(implicit ctx: Context): Boolean = {

    def suppressInline =
      ctx.owner.ownersIterator.exists(_.isTransparentMethod) ||
      ctx.settings.YnoInline.value ||
      ctx.isAfterTyper ||
      ctx.reporter.hasErrors

    hasBodyToInline(meth) && !suppressInline
  }

  /** Is `meth` a transparent method that should be inlined in this context? */
  def isTransparentInlineable(meth: Symbol)(implicit ctx: Context): Boolean =
    meth.isTransparentMethod && isInlineable(meth)

  /** Try to inline a call to a transparent method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @param pt     The expected type of the call.
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
    if (enclosingInlineds.length < ctx.settings.XmaxInlines.value) {
      val body = bodyToInline(tree.symbol) // can typecheck the tree and thereby produce errors
      if (ctx.reporter.hasErrors) tree
      else {
        val inlinerCtx =
          if (ctx.property(InlineBindings).isDefined) ctx
          else ctx.fresh.setProperty(InlineBindings, newMutableSymbolMap[Tree])
        new Inliner(tree, body)(inlinerCtx).inlined(pt)
      }
    }
    else errorTree(
      tree,
      i"""|Maximal number of successive inlines (${ctx.settings.XmaxInlines.value}) exceeded,
          |Maybe this is caused by a recursive transparent method?
          |You can use -Xmax:inlines to change the limit.""",
      (tree :: enclosingInlineds).last.pos
    )

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
    val reposition = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        super.transform(tree).withPos(inlined.call.pos)
      }
    }
    tpd.seq(inlined.bindings, reposition.transform(inlined.expansion))
  }
}

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call         the original call to an `inline` method
 *  @param  rhsToInline  the body of the transparent method that replaces the call.
 */
class Inliner(call: tpd.Tree, rhsToInline: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private val (methPart, targs, argss) = decomposeCall(call)
  private val meth = methPart.symbol
  private val prefix = qualifier(methPart)

  // Make sure all type arguments to the call are fully determined
  for (targ <- targs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

  /** A map from parameter names of the transparent method to references of the actual arguments.
   *  For a type argument this is the full argument type.
   *  For a value argument, it is a reference to either the argument value
   *  (if the argument is a pure expression of singleton type), or to `val` or `def` acting
   *  as a proxy (if the argument is something else).
   */
  private val paramBinding = new mutable.HashMap[Name, Type]

  /** A map from references to (type and value) parameters of the transparent method
   *  to their corresponding argument or proxy references, as given by `paramBinding`.
   */
  private val paramProxy = new mutable.HashMap[Type, Type]

  /** A map from the classes of (direct and outer) this references in `rhsToInline`
   *  to references of their proxies.
   *  Note that we can't index by the ThisType itself since there are several
   *  possible forms to express what is logicaly the same ThisType. E.g.
   *
   *     ThisType(TypeRef(ThisType(p), cls))
   *
   *  vs
   *
   *     ThisType(TypeRef(TermRef(ThisType(<root>), p), cls))
   *
   *  These are different (wrt ==) types but represent logically the same key
   */
  private val thisProxy = new mutable.HashMap[ClassSymbol, TermRef]

  /** A buffer for bindings that define proxies for actual arguments */
  val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]

  computeParamBindings(meth.info, targs, argss)

  private def newSym(name: Name, flags: FlagSet, info: Type): Symbol =
    ctx.newSymbol(ctx.owner, name, flags, info, coord = call.pos)

  /** A binding for the parameter of an inlined method. This is a `val` def for
   *  by-value parameters and a `def` def for by-name parameters. `val` defs inherit
   *  inline annotations from their parameters. The generated `def` is appended
   *  to `bindingsBuf`.
   *  @param name        the name of the parameter
   *  @param paramtp     the type of the parameter
   *  @param arg         the argument corresponding to the parameter
   *  @param bindingsBuf the buffer to which the definition should be appended
   */
  private def paramBindingDef(name: Name, paramtp: Type, arg: Tree,
                              bindingsBuf: mutable.ListBuffer[ValOrDefDef]): ValOrDefDef = {
    val argtpe = arg.tpe.dealiasKeepAnnots
    val isByName = paramtp.dealias.isInstanceOf[ExprType]
    val inlineFlag = if (paramtp.hasAnnotation(defn.TransparentParamAnnot)) Transparent else EmptyFlags
    val (bindingFlags, bindingType) =
      if (isByName) (Method, ExprType(argtpe.widen))
      else (inlineFlag, argtpe.widen)
    val boundSym = newSym(name, bindingFlags, bindingType).asTerm
    val binding =
      if (isByName) DefDef(boundSym, arg.changeOwner(ctx.owner, boundSym))
      else ValDef(boundSym, arg)
    bindingsBuf += binding
    binding
  }

  /** Populate `paramBinding` and `bindingsBuf` by matching parameters with
   *  corresponding arguments. `bindingbuf` will be further extended later by
   *  proxies to this-references.
   */
  private def computeParamBindings(tp: Type, targs: List[Tree], argss: List[List[Tree]]): Unit = tp match {
    case tp: PolyType =>
      (tp.paramNames, targs).zipped.foreach { (name, arg) =>
        paramBinding(name) = arg.tpe.stripTypeVar
      }
      computeParamBindings(tp.resultType, Nil, argss)
    case tp: MethodType =>
      (tp.paramNames, tp.paramInfos, argss.head).zipped.foreach { (name, paramtp, arg) =>
        paramBinding(name) = arg.tpe.dealias match {
          case _: SingletonType if isIdempotentExpr(arg) => arg.tpe
          case _ => paramBindingDef(name, paramtp, arg, bindingsBuf).symbol.termRef
        }
      }
      computeParamBindings(tp.resultType, targs, argss.tail)
    case _ =>
      assert(targs.isEmpty)
      assert(argss.isEmpty)
  }

  private def canElideThis(tpe: ThisType): Boolean =
    prefix.tpe == tpe && ctx.owner.isContainedIn(tpe.cls) ||
    tpe.cls.isContainedIn(meth) ||
    tpe.cls.is(Package)

  /** Populate `thisProxy` and `paramProxy` as follows:
   *
   *  1a. If given type refers to a static this, thisProxy binds it to corresponding global reference,
   *  1b. If given type refers to an instance this to a class that is not contained in the
   *      inlined method, create a proxy symbol and bind the thistype to refer to the proxy.
   *      The proxy is not yet entered in `bindingsBuf`; that will come later.
   *  2.  If given type refers to a parameter, make `paramProxy` refer to the entry stored
   *      in `paramNames` under the parameter's name. This roundabout way to bind parameter
   *      references to proxies is done because  we don't know a priori what the parameter
   *      references of a method are (we only know the method's type, but that contains TypeParamRefs
   *      and MethodParams, not TypeRefs or TermRefs.
   */
  private def registerType(tpe: Type): Unit = tpe match {
    case tpe: ThisType if !canElideThis(tpe) && !thisProxy.contains(tpe.cls) =>
      val proxyName = s"${tpe.cls.name}_this".toTermName
      val proxyType = tpe.asSeenFrom(prefix.tpe, meth.owner)
      thisProxy(tpe.cls) = newSym(proxyName, Synthetic, proxyType).termRef
      if (!tpe.cls.isStaticOwner)
        registerType(meth.owner.thisType) // make sure we have a base from which to outer-select
    case tpe: NamedType
    if tpe.symbol.is(Param) && tpe.symbol.owner == meth &&
       !paramProxy.contains(tpe) =>
      paramProxy(tpe) = paramBinding(tpe.name)
    case _ =>
  }

  /** Register type of leaf node */
  private def registerLeaf(tree: Tree): Unit = tree match {
    case _: This | _: Ident | _: TypeTree =>
      tree.tpe.foreachPart(registerType, stopAtStatic = true)
    case _ =>
  }

  /** The Inlined node representing the inlined call */
  def inlined(pt: Type) = {
    // make sure prefix is executed if it is impure
    if (!isIdempotentExpr(prefix)) registerType(meth.owner.thisType)

    // Register types of all leaves of inlined body so that the `paramProxy` and `thisProxy` maps are defined.
    rhsToInline.foreachSubTree(registerLeaf)

    // The class that the this-proxy `selfSym` represents
    def classOf(selfSym: Symbol) = selfSym.info.widen.classSymbol

    // The total nesting depth of the class represented by `selfSym`.
    def outerLevel(selfSym: Symbol): Int = classOf(selfSym).ownersIterator.length

    // All needed this-proxies, paired-with and sorted-by nesting depth of
    // the classes they represent (innermost first)
    val sortedProxies = thisProxy.toList.map {
      case (cls, proxy) => (outerLevel(cls), proxy.symbol)
    } sortBy (-_._1)

    // Compute val-definitions for all this-proxies and append them to `bindingsBuf`
    var lastSelf: Symbol = NoSymbol
    var lastLevel: Int = 0
    for ((level, selfSym) <- sortedProxies) {
      lazy val rhsClsSym = selfSym.info.widenDealias.classSymbol
      val rhs =
        if (lastSelf.exists)
          ref(lastSelf).outerSelect(lastLevel - level, selfSym.info)
        else if (rhsClsSym.is(Module) && rhsClsSym.isStatic)
          ref(rhsClsSym.sourceModule)
        else
          prefix
      bindingsBuf += ValDef(selfSym.asTerm, rhs)
      inlining.println(i"proxy at $level: $selfSym = ${bindingsBuf.last}")
      lastSelf = selfSym
      lastLevel = level
    }

    // The type map to apply to the inlined tree. This maps references to this-types
    // and parameters to type references of their arguments or proxies.
    val typeMap = new TypeMap {
      def apply(t: Type) = t match {
        case t: ThisType => thisProxy.getOrElse(t.cls, t)
        case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
        case t: SingletonType => paramProxy.getOrElse(t, mapOver(t))
        case t => mapOver(t)
      }
      override def mapClassInfo(tp: ClassInfo) = mapFullClassInfo(tp)
    }

    // The tree map to apply to the inlined tree. This maps references to this-types
    // and parameters to references of their arguments or their proxies.
    def treeMap(tree: Tree) = {
      tree match {
      case _: This =>
        tree.tpe match {
          case thistpe: ThisType =>
            thisProxy.get(thistpe.cls) match {
              case Some(t) => ref(t).withPos(tree.pos)
              case None => tree
            }
          case _ => tree
        }
      case _: Ident =>
        paramProxy.get(tree.tpe) match {
          case Some(t) if tree.isTerm && t.isSingleton => singleton(t).withPos(tree.pos)
          case Some(t) if tree.isType => TypeTree(t).withPos(tree.pos)
          case _ => tree
        }
      case _ => tree
    }}

    val inlineTyper = new InlineTyper
    val inlineCtx = inlineContext(call).fresh.setTyper(inlineTyper).setNewScope

    // The complete translation maps references to `this` and parameters to
    // corresponding arguments or proxies on the type and term level. It also changes
    // the owner from the inlined method to the current owner.
    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)(inlineCtx)

    val expansion = inliner.transform(rhsToInline.withPos(call.pos)) match {
      case Block(implicits, tpd.UntypedSplice(expansion)) =>
        val prevOwners = implicits.map(_.symbol.owner).distinct
        val localizer = new TreeTypeMap(oldOwners = prevOwners, newOwners = prevOwners.map(_ => ctx.owner))
        val (_, implicits1) = localizer.transformDefs(implicits)
        for (idef <- implicits1) {
          bindingsBuf += idef.withType(idef.symbol.typeRef).asInstanceOf[ValOrDefDef]
            // Note: Substituting new symbols does not automatically lead to good prefixes
            // if the previous symbol was owned by a class. That's why we need to set the type
            // of `idef` explicitly. It would be nice if substituters were smarter, but
            // it seems non-trivial to come up with rules that work in all cases.
          inlineCtx.enter(idef.symbol)
        }
        expansion
      case tpd.UntypedSplice(expansion) =>
        expansion
      case expansion =>
        expansion
    }

    /** If this is a value binding:
     *   - reduce its rhs if it is a projection and adjust its type accordingly,
     *   - record symbol -> rhs in the InlineBindings context propery.
     *  Also, set position to the one of the inline call.
     */
    def normalizeBinding(binding: ValOrDefDef)(implicit ctx: Context) = {
      val binding1 = binding match {
        case binding: ValDef =>
          val rhs1 = reduceProjection(binding.rhs)
          inlineBindings(inlineCtx).put(binding.symbol, rhs1)
          if (rhs1 `eq` binding.rhs) binding
          else {
            binding.symbol.info = rhs1.tpe
            cpy.ValDef(binding)(tpt = TypeTree(rhs1.tpe), rhs = rhs1)
          }
        case _ =>
          binding
      }
      binding1.withPos(call.pos)
    }

    trace(i"inlining $call", inlining, show = true) {

      /** All bindings in `bindingsBuf` */
      val bindings = bindingsBuf.toList.map(normalizeBinding)

      // The final expansion runs a typing pass over the inlined tree. See InlineTyper for details.
      val expansion1 = inlineTyper.typed(expansion, pt)(inlineCtx)

      if (ctx.settings.verbose.value) {
        inlining.println(i"to inline = $rhsToInline")
        inlining.println(i"original bindings = $bindings%\n%")
        inlining.println(i"original expansion = $expansion1")
      }

      val (finalBindings, finalExpansion) = dropUnusedDefs(bindings, expansion1)

      tpd.Inlined(call, finalBindings, finalExpansion)
    }
  }

  /** An extractor for terms equivalent to `new C(args)`, returning the class `C`
   *  and the arguments `args`. Can see inside blocks and Inlined nodes and can
   *  follow a reference to an inline value binding to its right hand side.
   */
  object NewInstance {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Symbol, List[Tree], List[Tree])] = {
      def unapplyLet(bindings: List[Tree], expr: Tree) =
        unapply(expr) map {
          case (cls, reduced, prefix) => (cls, reduced, bindings ::: prefix)
        }
      tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          Some((tpt.tpe.classSymbol, args, Nil))
        case Ident(_) =>
          inlineBindings.get(tree.symbol).flatMap(unapply)
        case Inlined(_, bindings, expansion) =>
          unapplyLet(bindings, expansion)
        case Block(stats, expr) if isPureExpr(tree) =>
          unapplyLet(stats, expr)
        case _ =>
          None
      }
    }
  }

  /** If we are inlining a transparent method and `tree` is equivalent to `new C(args).x`
   *  where class `C` does not have initialization code and `x` is a parameter corresponding
   *  to one of the arguments `args`, the corresponding argument, prefixed by the evaluation
   *  of impure arguments, otherwise `tree` itself.
   */
  def reduceProjection(tree: Tree)(implicit ctx: Context): Tree = {
    if (meth.isTransparentMethod) {
      if (ctx.debug) inlining.println(i"try reduce projection $tree")
      tree match {
        case Select(NewInstance(cls, args, prefix), field) if cls.isNoInitsClass =>
          def matches(param: Symbol, selection: Symbol): Boolean =
            param == selection || {
              selection.name match {
                case InlineAccessorName(underlying) =>
                  param.name == underlying && selection.info.isInstanceOf[ExprType]
                case _ =>
                  false
              }
            }
          val idx = cls.asClass.paramAccessors.indexWhere(matches(_, tree.symbol))
          if (idx >= 0 && idx < args.length) {
            def collectImpure(from: Int, end: Int) =
              (from until end).filterNot(i => isPureExpr(args(i))).toList.map(args)
            val leading = collectImpure(0, idx)
            val trailing = collectImpure(idx + 1, args.length)
            val arg = args(idx)
            val argInPlace =
              if (trailing.isEmpty) arg
              else letBindUnless(TreeInfo.Pure, arg)(seq(trailing, _))
            val reduced = seq(prefix, seq(leading, argInPlace))
            inlining.println(i"projecting $tree -> ${reduced}")
            return reduced
          }
        case _ =>
      }
    }
    tree
  }

  /** An extractor for references to inlineable arguments. These are :
   *   - by-value arguments marked with `inline`
   *   - all by-name arguments
   */
  private object InlineableArg {
    lazy val paramProxies = paramProxy.values.toSet
    def unapply(tree: Trees.Ident[_])(implicit ctx: Context): Option[Tree] =
      if (paramProxies.contains(tree.typeOpt))
        bindingsBuf.find(_.name == tree.name) match {
          case Some(vdef: ValDef) if vdef.symbol.is(Transparent) =>
            Some(vdef.rhs.changeOwner(vdef.symbol, ctx.owner))
          case Some(ddef: DefDef) =>
            Some(ddef.rhs.changeOwner(ddef.symbol, ctx.owner))
          case _ => None
        }
      else None
  }

  /** A typer for inlined bodies of transparent methods. Beyond standard typing
   *  an inline typer performs the following functions:
   *
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are by-name closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  class InlineTyper extends Typer {

    protected def tryInline(tree: tpd.Tree)(implicit ctx: Context) = tree match {
      case InlineableArg(rhs) =>
        inlining.println(i"inline arg $tree -> $rhs")
        rhs
      case _ =>
        EmptyTree
    }

    override def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
      tpe match {
        case tpe: NamedType if tpe.symbol.exists && !tpe.symbol.isAccessibleFrom(tpe.prefix, superAccess) =>
          tpe.info match {
            case TypeAlias(alias) => return ensureAccessible(alias, superAccess, pos)
            case info: ConstantType if tpe.symbol.isStable => return info
            case _ =>
          }
        case _ =>
      }
      super.ensureAccessible(tpe, superAccess, pos)
    }

    override def typedTypedSplice(tree: untpd.TypedSplice)(implicit ctx: Context): Tree =
      reduceProjection(tryInline(tree.splice) `orElse` super.typedTypedSplice(tree))

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context) =
      constToLiteral(reduceProjection(super.typedSelect(tree, pt)))

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) = {
      val cond1 = typed(tree.cond, defn.BooleanType)
      cond1.tpe.widenTermRefExpr match {
        case ConstantType(Constant(condVal: Boolean)) =>
          var selected = typed(if (condVal) tree.thenp else tree.elsep, pt)
          if (selected.isEmpty) selected = tpd.Literal(Constant(()))
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case _ =>
          val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
          super.typedIf(if1, pt)
      }
    }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) = {

      /** Rewrite an application
       *
       *    ((x1, ..., sn) => b)(e1, ..., en)
       *
       *  to
       *
       *    val/def x1 = e1; ...; val/def xn = en; b
       *
       *  where `def` is used for call-by-name parameters. However, we shortcut any NoPrefix
       *  refs among the ei's directly without creating an intermediate binding.
       */
      def betaReduce(tree: Tree) = tree match {
        case Apply(Select(cl @ closureDef(ddef), nme.apply), args) if defn.isFunctionType(cl.tpe) =>
          ddef.tpe.widen match {
            case mt: MethodType if ddef.vparamss.head.length == args.length =>
              val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]
              val argSyms = (mt.paramNames, mt.paramInfos, args).zipped.map { (name, paramtp, arg) =>
                arg.tpe.dealias match {
                  case ref @ TermRef(NoPrefix, _) => ref.symbol
                  case _ => paramBindingDef(name, paramtp, arg, bindingsBuf).symbol
                }
              }
              val expander = new TreeTypeMap(
                oldOwners = ddef.symbol :: Nil,
                newOwners = ctx.owner :: Nil,
                substFrom = ddef.vparamss.head.map(_.symbol),
                substTo = argSyms)
              Block(bindingsBuf.toList, expander.transform(ddef.rhs))
            case _ => tree
          }
        case _ => tree
      }

      constToLiteral(betaReduce(super.typedApply(tree, pt)))
    }

    override def newLikeThis: Typer = new InlineTyper
  }

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  def dropUnusedDefs(bindings: List[ValOrDefDef], tree: Tree)(implicit ctx: Context): (List[ValOrDefDef], Tree) = {
    val refCount = newMutableSymbolMap[Int]
    val bindingOfSym = newMutableSymbolMap[ValOrDefDef]
    def isInlineable(binding: ValOrDefDef) = binding match {
      case DefDef(_, Nil, Nil, _, _) => true
      case vdef @ ValDef(_, _, _) => isPureExpr(vdef.rhs)
      case _ => false
    }
    for (binding <- bindings if isInlineable(binding)) {
      refCount(binding.symbol) = 0
      bindingOfSym(binding.symbol) = binding
    }
    val countRefs = new TreeTraverser {
      override def traverse(t: Tree)(implicit ctx: Context) = {
        t match {
          case t: RefTree =>
            refCount.get(t.symbol) match {
              case Some(x) => refCount(t.symbol) = x + 1
              case none =>
            }
          case _: New | _: TypeTree =>
            t.tpe.foreachPart {
              case ref: TermRef =>
                refCount.get(ref.symbol) match {
                  case Some(x) => refCount(ref.symbol) = x + 2
                  case none =>
                }
              case _ =>
            }
          case _ =>
        }
        traverseChildren(t)
      }
    }
    countRefs.traverse(tree)
    for (binding <- bindings) countRefs.traverse(binding.rhs)
    val inlineBindings = new TreeMap {
      override def transform(t: Tree)(implicit ctx: Context) =
        super.transform {
          t match {
            case t: RefTree =>
              val sym = t.symbol
              refCount.get(sym) match {
                case Some(1) if sym.is(Method) =>
                  bindingOfSym(sym).rhs.changeOwner(sym, ctx.owner)
                case none => t
              }
            case _ => t
          }
        }
      }
    def retain(binding: ValOrDefDef) = refCount.get(binding.symbol) match {
      case Some(x) => x > 1 || x == 1 && !binding.symbol.is(Method)
      case none => true
    }
    val retained = bindings.filterConserve(retain)
    if (retained `eq` bindings) {
      (bindings, tree)
    }
    else {
      val expanded = inlineBindings.transform(tree)
      dropUnusedDefs(retained, expanded)
    }
  }
}
