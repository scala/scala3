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
import NameKinds.{ClassifiedNameKind, InlineAccessorName, UniqueInlineName, TransparentScrutineeName, TransparentBinderName}
import ProtoTypes.selectionProto
import SymDenotations.SymDenotation
import Annotations._
import transform.{ExplicitOuter, AccessProxies}
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import collection.mutable
import transform.TypeUtils._
import transform.SymUtils._
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
    sym.isTransparentInlineable && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.unforcedAnnotation(defn.BodyAnnot).get.tree

  /** Should call with method `meth` be inlined in this context? */
  def isInlineable(meth: Symbol)(implicit ctx: Context): Boolean = {

    def suppressInline =
      ctx.inTransparentMethod ||
      ctx.settings.YnoInline.value ||
      ctx.isAfterTyper ||
      ctx.reporter.hasErrors

    hasBodyToInline(meth) && !suppressInline
  }

  /** Should call be inlined in this context? */
  def isInlineable(tree: Tree)(implicit ctx: Context): Boolean = tree match {
    case Block(_, expr) => isInlineable(expr)
    case _ => isInlineable(tree.symbol)
  }

  /** Is `meth` a transparent method that should be inlined in this context? */
  def isTransparentInlineable(meth: Symbol)(implicit ctx: Context): Boolean =
    meth.isTransparentInlineable && isInlineable(meth)

  /** Try to inline a call to a transparent method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @param pt     The expected type of the call.
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {

	/** Set the position of all trees logically contained in the expansion of
	 *  inlined call `call` to the position of `call`. This transform is necessary
	 *  when lifting bindings from the expansion to the outside of the call.
	 */
    def liftFromInlined(call: Tree) = new TreeMap {
      override def transform(t: Tree)(implicit ctx: Context) = {
        t match {
          case Inlined(t, Nil, expr) if t.isEmpty => expr
          case _ => super.transform(t.withPos(call.pos))
        }
      }
    }

    val bindings = new mutable.ListBuffer[Tree]

    /** Lift bindings around inline call or in its function part to
     *  the `bindings` buffer. This is done as an optimization to keep
     *  inline call expansions smaller.
     */
    def liftBindings(tree: Tree, liftPos: Tree => Tree): Tree = tree match {
      case Block(stats, expr) =>
        bindings ++= stats.map(liftPos)
        liftBindings(expr, liftPos)
      case Inlined(call, stats, expr) =>
        bindings ++= stats.map(liftPos)
        val lifter = liftFromInlined(call)
        cpy.Inlined(tree)(call, Nil, liftBindings(expr, liftFromInlined(call).transform(_)))
      case Apply(fn, args) =>
        cpy.Apply(tree)(liftBindings(fn, liftPos), args)
      case TypeApply(fn, args) =>
        cpy.TypeApply(tree)(liftBindings(fn, liftPos), args)
      case Select(qual, name) =>
        cpy.Select(tree)(liftBindings(qual, liftPos), name)
      case _ =>
        tree
    }

    val tree1 = liftBindings(tree, identity)
    if (bindings.nonEmpty)
      cpy.Block(tree)(bindings.toList, inlineCall(tree1, pt))
    else if (enclosingInlineds.length < ctx.settings.XmaxInlines.value) {
      val body = bodyToInline(tree.symbol) // can typecheck the tree and thereby produce errors
      if (ctx.reporter.hasErrors) tree
      else {
        val inlinerCtx =
          if (ctx.property(InlineBindings).isDefined) ctx
          else ctx.fresh.setProperty(InlineBindings, newMutableSymbolMap[Tree])
        new Inliner(tree, body)(inlinerCtx).inlined(pt)
      }
    }
    else
      errorTree(
        tree,
        i"""|Maximal number of successive inlines (${ctx.settings.XmaxInlines.value}) exceeded,
            |Maybe this is caused by a recursive transparent method?
            |You can use -Xmax:inlines to change the limit.""",
        (tree :: enclosingInlineds).last.pos
      )
  }

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
    if (enclosingInlineds.nonEmpty) inlined // Remove in the outer most inlined call
    else {
      // Position used for any tree that was inlined (including recursive inlines)
      val inlinedAtPos = inlined.call.pos

      /** Removes all Inlined trees, replacing them with blocks.
       *  Repositions all trees directly inside an inlined expantion of a non empty call to the position of the call.
       *  Any tree directly inside an empty call (inlined in the inlined code) retains their position.
       */
      class Reposition extends TreeMap {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = {
          tree match {
            case tree: Inlined => transformInline(tree)
            case _ =>
              val transformed = super.transform(tree)
              enclosingInlineds match {
                case call :: _ if call.symbol.topLevelClass != ctx.owner.topLevelClass =>
                  // reposition tree inlined from some other file
                  transformed.withPos(inlinedAtPos)
                case _ => transformed
              }
          }
        }
        def transformInline(tree: tpd.Inlined)(implicit ctx: Context): Tree = {
          tpd.seq(transformSub(tree.bindings), transform(tree.expansion)(inlineContext(tree.call)))
        }
      }

      (new Reposition).transformInline(inlined)
    }
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

  private val (methPart, callTypeArgs, callValueArgss) = decomposeCall(call)
  private val inlinedMethod = methPart.symbol
  private val inlineCallPrefix = qualifier(methPart)

  // Make sure all type arguments to the call are fully determined
  for (targ <- callTypeArgs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

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
  private val bindingsBuf = new mutable.ListBuffer[MemberDef]

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
                              bindingsBuf: mutable.ListBuffer[MemberDef]): MemberDef = {
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

  // Compute val-definitions for all this-proxies and append them to `bindingsBuf`
  private def computeThisBindings() = {
    // All needed this-proxies, paired-with and sorted-by nesting depth of
    // the classes they represent (innermost first)
    val sortedProxies = thisProxy.toList.map {
      case (cls, proxy) =>
        // The class that the this-proxy `selfSym` represents
        def classOf(selfSym: Symbol) = selfSym.info.widen.classSymbol
        // The total nesting depth of the class represented by `selfSym`.
        def outerLevel(selfSym: Symbol): Int = classOf(selfSym).ownersIterator.length
        (outerLevel(cls), proxy.symbol)
    }.sortBy(-_._1)

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
          inlineCallPrefix
      bindingsBuf += ValDef(selfSym.asTerm, rhs)
      inlining.println(i"proxy at $level: $selfSym = ${bindingsBuf.last}")
      lastSelf = selfSym
      lastLevel = level
    }
  }

  private def canElideThis(tpe: ThisType): Boolean =
    inlineCallPrefix.tpe == tpe && ctx.owner.isContainedIn(tpe.cls) ||
    tpe.cls.isContainedIn(inlinedMethod) ||
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
      val proxyType = tpe.asSeenFrom(inlineCallPrefix.tpe, inlinedMethod.owner)
      thisProxy(tpe.cls) = newSym(proxyName, Synthetic, proxyType).termRef
      if (!tpe.cls.isStaticOwner)
        registerType(inlinedMethod.owner.thisType) // make sure we have a base from which to outer-select
    case tpe: NamedType
    if tpe.symbol.is(Param) && tpe.symbol.owner == inlinedMethod && !paramProxy.contains(tpe) =>
      paramProxy(tpe) = paramBinding(tpe.name)
    case _ =>
  }

  /** Register type of leaf node */
  private def registerLeaf(tree: Tree): Unit = tree match {
    case _: This | _: Ident | _: TypeTree =>
      tree.tpe.foreachPart(registerType, stopAtStatic = true)
    case _ =>
  }

  /** Make `tree` part of inlined expansion. This means its owner has to be changed
   *  from its `originalOwner`, and, if it comes from outside the inlined method
   *  itself, it has to be marked as an inlined argument.
   */
  def integrate(tree: Tree, originalOwner: Symbol)(implicit ctx: Context) = {
    val result = tree.changeOwner(originalOwner, ctx.owner)
    if (!originalOwner.isContainedIn(inlinedMethod)) Inlined(EmptyTree, Nil, result)
    else result
  }

  /** The Inlined node representing the inlined call */
  def inlined(pt: Type) = {
    // Compute bindings for all parameters, appending them to bindingsBuf
    computeParamBindings(inlinedMethod.info, callTypeArgs, callValueArgss)

    // make sure prefix is executed if it is impure
    if (!isIdempotentExpr(inlineCallPrefix)) registerType(inlinedMethod.owner.thisType)

    // Register types of all leaves of inlined body so that the `paramProxy` and `thisProxy` maps are defined.
    rhsToInline.foreachSubTree(registerLeaf)

    // Compute bindings for all this-proxies, appending them to bindingsBuf
    computeThisBindings()

    val inlineTyper = new InlineTyper
    val inlineCtx = inlineContext(call).fresh.setTyper(inlineTyper).setNewScope

    // A tree type map to prepare the inlined body for typechecked.
    // The translation maps references to `this` and parameters to
    // corresponding arguments or proxies on the type and term level. It also changes
    // the owner from the inlined method to the current owner.
    val inliner = new TreeTypeMap(
      typeMap =
        new TypeMap {
          def apply(t: Type) = t match {
            case t: ThisType => thisProxy.getOrElse(t.cls, t)
            case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
            case t: SingletonType => paramProxy.getOrElse(t, mapOver(t))
            case t => mapOver(t)
          }
          override def mapClassInfo(tp: ClassInfo) = mapFullClassInfo(tp)
        },
      treeMap = {
        case tree: This =>
          tree.tpe match {
            case thistpe: ThisType =>
              thisProxy.get(thistpe.cls) match {
                case Some(t) => ref(t).withPos(tree.pos)
                case None => tree
              }
            case _ => tree
          }
        case tree: Ident =>
          paramProxy.get(tree.tpe) match {
            case Some(t) if tree.isTerm && t.isSingleton => singleton(t).withPos(tree.pos)
            case Some(t) if tree.isType => TypeTree(t).withPos(tree.pos)
            case _ => tree
          }
        case tree => tree
      },
      oldOwners = inlinedMethod :: Nil,
      newOwners = ctx.owner :: Nil
    )(inlineCtx)

    // Apply inliner to `rhsToInline`, split off any implicit bindings from result, and
    // make them part of `bindingsBuf`. The expansion is then the untyped tree that remains.
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

    trace(i"inlining $call", inlining, show = true) {

      // The normalized bindings collected in `bindingsBuf`
      bindingsBuf.transform(reducer.normalizeBinding(_)(inlineCtx))

      // Run a typing pass over the inlined tree. See InlineTyper for details.
      val expansion1 = inlineTyper.typed(expansion, pt)(inlineCtx)

      if (ctx.settings.verbose.value) {
        inlining.println(i"to inline = $rhsToInline")
        inlining.println(i"original bindings = ${bindingsBuf.toList}%\n%")
        inlining.println(i"original expansion = $expansion1")
      }

      // Drop unused bindings
      val matchBindings = reducer.matchBindingsBuf.toList
      val (finalBindings, finalExpansion) = dropUnusedDefs(bindingsBuf.toList ++ matchBindings, expansion1)
      val (finalMatchBindings, finalArgBindings) = finalBindings.partition(matchBindings.contains(_))

      // Take care that only argument bindings go into `bindings`, since positions are
      // different for bindings from arguments and bindings from body.
      tpd.Inlined(call, finalArgBindings, seq(finalMatchBindings, finalExpansion))
    }
  }

  /** A utility object offering methods for rewriting inlined code */
  object reducer {

    /** Additional bindings established by reducing match expressions */
    val matchBindingsBuf = new mutable.ListBuffer[MemberDef]

    /** An extractor for terms equivalent to `new C(args)`, returning the class `C`,
     *  a list of bindings, and the arguments `args`. Can see inside blocks and Inlined nodes and can
     *  follow a reference to an inline value binding to its right hand side.
     *  @return    optionally, a triple consisting of
     *             - the class `C`
     *             - the arguments `args`
     *             - any bindings that wrap the instance creation
     */
    private object NewInstance {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Symbol, List[Tree], List[Tree])] = {
        def unapplyLet(bindings: List[Tree], expr: Tree) =
          unapply(expr) map {
            case (cls, reduced, prefix) => (cls, reduced, bindings ::: prefix)
          }
        tree match {
          case Apply(fn, args) =>
            fn match {
              case Select(New(tpt), nme.CONSTRUCTOR) =>
                Some((tpt.tpe.classSymbol, args, Nil))
              case TypeApply(Select(New(tpt), nme.CONSTRUCTOR), _) =>
                Some((tpt.tpe.classSymbol, args, Nil))
              case _ =>
                val meth = fn.symbol
                if (meth.name == nme.apply &&
                    meth.flags.is(Synthetic) &&
                    meth.owner.linkedClass.is(Case)) Some(meth.owner.linkedClass, args, Nil)
                else None
            }
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
            val fullArg = seq(prefix, seq(leading, argInPlace))
            new TreeTypeMap().transform(fullArg) // make sure local bindings in argument have fresh symbols
              .reporting(res => i"projecting $tree -> $res", inlining)
          }
          else tree
        case Block(stats, expr) if stats.forall(isPureBinding) =>
          cpy.Block(tree)(stats, reduceProjection(expr))
        case _ => tree
      }
    }

    /** If this is a value binding:
     *   - reduce its rhs if it is a projection and adjust its type accordingly,
     *   - record symbol -> rhs in the InlineBindings context propery.
     */
    def normalizeBinding(binding: MemberDef)(implicit ctx: Context) = {
      val binding1 = binding match {
        case binding: ValDef =>
          val rhs1 = reduceProjection(binding.rhs)
          inlineBindings(ctx).put(binding.symbol, rhs1)
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

    /** An extractor for references to inlineable arguments. These are :
     *   - by-value arguments marked with `inline`
     *   - all by-name arguments
     */
    private object InlineableArg {
      lazy val paramProxies = paramProxy.values.toSet
      def unapply(tree: Trees.Ident[_])(implicit ctx: Context): Option[Tree] = {
        def search(buf: mutable.ListBuffer[MemberDef]) = buf.find(_.name == tree.name)
        if (paramProxies.contains(tree.typeOpt))
          search(bindingsBuf).orElse(search(matchBindingsBuf)) match {
            case Some(vdef: ValDef) if vdef.symbol.is(Transparent) =>
              Some(integrate(vdef.rhs, vdef.symbol))
            case Some(ddef: DefDef) =>
              Some(integrate(ddef.rhs, ddef.symbol))
            case _ => None
          }
        else None
      }
    }

    object ConstantValue {
      def unapply(tree: Tree)(implicit ctx: Context) = tree.tpe.widenTermRefExpr match {
        case ConstantType(Constant(x)) => Some(x)
        case _ => None
      }
    }

    def tryInline(tree: tpd.Tree)(implicit ctx: Context) = tree match {
      case InlineableArg(rhs) =>
        inlining.println(i"inline arg $tree -> $rhs")
        rhs
      case _ =>
        EmptyTree
    }

    /** Rewrite an application
      *
      *    ((x1, ..., xn) => b)(e1, ..., en)
      *
      *  to
      *
      *    val/def x1 = e1; ...; val/def xn = en; b
      *
      *  where `def` is used for call-by-name parameters. However, we shortcut any NoPrefix
      *  refs among the ei's directly without creating an intermediate binding.
      */
    def betaReduce(tree: Tree)(implicit ctx: Context) = tree match {
      case Apply(Select(cl @ closureDef(ddef), nme.apply), args) if defn.isFunctionType(cl.tpe) =>
        ddef.tpe.widen match {
          case mt: MethodType if ddef.vparamss.head.length == args.length =>
            val bindingsBuf = new mutable.ListBuffer[MemberDef]
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

    /** The result type of reducing a match. It consists, optionally of a list of bindings
     *  for the pattern-bound variables and the RHS of the selected case.
     *  Returns `None` if not case was selected.
     */
    type MatchRedux = Option[(List[MemberDef], untpd.Tree)]

    /** Reduce a toplevel match of a transparent function
     *   @param     scrutinee    the scrutinee expression, assumed to be pure
     *   @param     scrutType    its fully defined type
     *   @param     cases        All cases of the match
     *   @param     typer        The current inline typer
     *   @return    optionally, if match can be reduced to a matching case: A pair of
     *              bindings for all pattern-bound variables and the untyped RHS of the case.
     */
    def reduceTopLevelMatch(scrutinee: Tree, scrutType: Type, cases: List[untpd.CaseDef], typer: Typer)(implicit ctx: Context): MatchRedux = {

      val gadtSyms = typer.gadtSyms(scrutType)

      /** Try to match pattern `pat` against scrutinee reference `scrut`. If successful add
       *  bindings for variables bound in this pattern to `bindingsBuf`.
       */
      def reducePattern(bindingsBuf: mutable.ListBuffer[MemberDef], scrut: TermRef, pat: Tree): Boolean = {
        val isImplicit = scrut.info == defn.ImplicitScrutineeTypeRef

        def newBinding(name: TermName, flags: FlagSet, rhs: Tree): Symbol = {
          val info = if (flags `is` Implicit) rhs.tpe.widen else rhs.tpe.widenTermRefExpr
          val sym = newSym(name, flags, info).asTerm
          bindingsBuf += ValDef(sym, constToLiteral(rhs))
          sym
        }

        def searchImplicit(name: TermName, tpt: Tree) = {
          val evidence = typer.inferImplicitArg(tpt.tpe, tpt.pos)
          evidence.tpe match {
            case fail: Implicits.AmbiguousImplicits =>
              ctx.error(typer.missingArgMsg(evidence, tpt.tpe, ""), tpt.pos)
              true // hard error: return true to stop implicit search here
            case fail: Implicits.SearchFailureType =>
              false
            case _ =>
              if (name != nme.WILDCARD) newBinding(name, Implicit, evidence)
              true
          }
        }

        pat match {
          case Typed(pat1, tpt) =>
            if (isImplicit) searchImplicit(nme.WILDCARD, tpt)
            else scrut <:< tpt.tpe && reducePattern(bindingsBuf, scrut, pat1)
          case pat @ Bind(name: TermName, Typed(_, tpt)) if isImplicit =>
            searchImplicit(name, tpt)
          case pat @ Bind(name: TermName, body) =>
            reducePattern(bindingsBuf, scrut, body) && {
              if (name != nme.WILDCARD) newBinding(name, EmptyFlags, ref(scrut))
              true
            }
          case Ident(nme.WILDCARD) =>
            true
          case pat: Literal =>
            scrut.widenTermRefExpr =:= pat.tpe
          case pat: RefTree =>
            scrut =:= pat.tpe ||
            scrut.widen.classSymbol.is(Module) && scrut.widen =:= pat.tpe.widen && {
              scrut.prefix match {
                case _: SingletonType | NoPrefix => true
                case _ => false
              }
            }
          case UnApply(unapp, _, pats) =>
            unapp.tpe.widen match {
              case mt: MethodType if mt.paramInfos.length == 1 =>
                val paramType = mt.paramInfos.head
                val paramCls = paramType.classSymbol
                paramCls.is(Case) && unapp.symbol.is(Synthetic) && scrut <:< paramType && {
                  val caseAccessors =
                    if (paramCls.is(Scala2x)) paramCls.caseAccessors.filter(_.is(Method))
                    else paramCls.asClass.paramAccessors
                  var subOK = caseAccessors.length == pats.length
                  for ((pat, accessor) <- (pats, caseAccessors).zipped)
                    subOK = subOK && {
                      val rhs = constToLiteral(reduceProjection(ref(scrut).select(accessor).ensureApplied))
                      val elem = newBinding(TransparentBinderName.fresh(), Synthetic, rhs)
                      reducePattern(bindingsBuf, elem.termRef, pat)
                    }
                  subOK
                }
              case _ =>
                false
            }
          case _ => false
        }
      }

      /** The initial scrutinee binding: `val $scrutineeN = <scrutinee>` */
      val scrutineeSym = newSym(TransparentScrutineeName.fresh(), Synthetic, scrutType).asTerm
      val scrutineeBinding = normalizeBinding(ValDef(scrutineeSym, scrutinee))

      def reduceCase(cdef: untpd.CaseDef): MatchRedux = {
        def guardOK = cdef.guard.isEmpty || {
          typer.typed(cdef.guard, defn.BooleanType) match {
            case ConstantValue(true) => true
            case _ => false
          }
        }
        val caseBindingsBuf = new mutable.ListBuffer[MemberDef]()
        if (scrutType != defn.ImplicitScrutineeTypeRef) caseBindingsBuf += scrutineeBinding
        val pat1 = typer.typedPattern(cdef.pat, scrutType)(typer.gadtContext(gadtSyms))
        if (reducePattern(caseBindingsBuf, scrutineeSym.termRef, pat1) && guardOK)
          Some((caseBindingsBuf.toList, cdef.body))
        else
          None
      }

      def recur(cases: List[untpd.CaseDef]): MatchRedux = cases match {
        case Nil => None
        case cdef :: cases1 => reduceCase(cdef) `orElse` recur(cases1)
      }

      recur(cases)
    }
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
    import reducer._

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

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) =
      typed(tree.cond, defn.BooleanType) match {
        case cond1 @ ConstantValue(b: Boolean) =>
          val selected0 = if (b) tree.thenp else tree.elsep
          val selected = if (selected0.isEmpty) tpd.Literal(Constant(())) else typed(selected0, pt)
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case cond1 =>
          val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
          super.typedIf(if1, pt)
      }

    override def typedMatchFinish(tree: untpd.Match, sel: Tree, selType: Type, pt: Type)(implicit ctx: Context) =
      tree.getAttachment(PrepareTransparent.TopLevelMatch) match {
        case Some(_) if !ctx.owner.isTransparentMethod => // don't reduce match of nested transparent yet
          reduceTopLevelMatch(sel, sel.tpe, tree.cases, this) match {
            case Some((caseBindings, rhs)) =>
              var rhsCtx = ctx.fresh.setNewScope
              for (binding <- caseBindings) {
                matchBindingsBuf += binding
                rhsCtx.enter(binding.symbol)
              }
              typedExpr(rhs, pt)(rhsCtx)
            case None =>
              def guardStr(guard: untpd.Tree) = if (guard.isEmpty) "" else i" if $guard"
              def patStr(cdef: untpd.CaseDef) = i"case ${cdef.pat}${guardStr(cdef.guard)}"
              errorTree(tree, em"""cannot reduce top-level match of transparent function with
                                  | scrutinee:  $sel : ${sel.tpe}
                                  | patterns :  ${tree.cases.map(patStr).mkString("\n             ")}
                                  |
                                  | Hint: if you do not expect the match to be reduced, put it in a locally { ... } block.""")
          }
        case _ =>
          super.typedMatchFinish(tree, sel, selType, pt)
      }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) =
      constToLiteral(betaReduce(super.typedApply(tree, pt)))

    override def newLikeThis: Typer = new InlineTyper
  }

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  def dropUnusedDefs(bindings: List[MemberDef], tree: Tree)(implicit ctx: Context): (List[MemberDef], Tree) = {
    val refCount = newMutableSymbolMap[Int]
    val bindingOfSym = newMutableSymbolMap[MemberDef]
    def isInlineable(binding: MemberDef) = binding match {
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
        def updateRefCount(sym: Symbol, inc: Int) =
          for (x <- refCount.get(sym)) refCount(sym) = x + inc
        t match {
          case t: RefTree => updateRefCount(t.symbol, 1)
          case _: New | _: TypeTree =>
            t.typeOpt.foreachPart {
              case ref: TermRef => updateRefCount(ref.symbol, 2) // can't be inlined, so make sure refCount is at least 2
              case _ =>
            }
          case _ =>
        }
        traverseChildren(t)
      }
    }
    countRefs.traverse(tree)
    for (binding <- bindings) countRefs.traverse(binding)

    def retain(boundSym: Symbol) = {
      refCount.get(boundSym) match {
        case Some(x) => x > 1 || x == 1 && !boundSym.is(Method)
        case none => true
      }
    } && !boundSym.is(TransparentImplicitMethod)

    val inlineBindings = new TreeMap {
      override def transform(t: Tree)(implicit ctx: Context) = t match {
        case t: RefTree =>
          val sym = t.symbol
          val t1 = refCount.get(sym) match {
            case Some(1) =>
              bindingOfSym(sym) match {
                case binding: ValOrDefDef => integrate(binding.rhs, sym)
              }
            case none => t
          }
          super.transform(t1)
        case t: Apply =>
          val t1 = super.transform(t)
          if (t1 `eq` t) t else reducer.betaReduce(t1)
        case _ =>
          super.transform(t)
      }
    }

    val retained = bindings.filterConserve(binding => retain(binding.symbol))
    if (retained `eq` bindings) {
      (bindings, tree)
    }
    else {
      val expanded = inlineBindings.transform(tree)
      dropUnusedDefs(retained, expanded)
    }
  }
}
