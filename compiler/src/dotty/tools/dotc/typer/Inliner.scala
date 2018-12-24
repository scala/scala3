package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.{Trees, untpd, tpd, TreeTypeMap}
import Trees._
import core._
import Flags._
import Symbols._
import Types._
import Decorators._
import Constants._
import StdNames._
import transform.SymUtils._
import Contexts.Context
import Names.{Name, TermName}
import NameKinds.{InlineAccessorName, InlineScrutineeName, InlineBinderName}
import ProtoTypes.selectionProto
import SymDenotations.SymDenotation
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import collection.mutable
import reporting.trace
import util.Positions.Position
import ast.TreeInfo

object Inliner {
  import tpd._

  /** `sym` is an inline method with a known body to inline (note: definitions coming
   *  from Scala2x class files might be `@forceInline`, but still lack that body).
   */
  def hasBodyToInline(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isInlineMethod && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.unforcedAnnotation(defn.BodyAnnot).get.tree

  /** Should call to method `meth` be inlined in this context? */
  def isInlineable(meth: Symbol)(implicit ctx: Context): Boolean =
    meth.is(Inline) && hasBodyToInline(meth) && !ctx.inInlineMethod

  /** Should call be inlined in this context? */
  def isInlineable(tree: Tree)(implicit ctx: Context): Boolean = tree match {
    case Block(_, expr) => isInlineable(expr)
    case _ => isInlineable(tree.symbol) && !tree.tpe.isInstanceOf[MethodOrPoly]
  }

  /** Try to inline a call to an inline method. Fail with error if the maximal
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
      else new Inliner(tree, body).inlined(pt)
    }
    else
      errorTree(
        tree,
        i"""|Maximal number of successive inlines (${ctx.settings.XmaxInlines.value}) exceeded,
            |Maybe this is caused by a recursive inline method?
            |You can use -Xmax-inlines to change the limit.""",
        (tree :: enclosingInlineds).last.pos
      )
  }

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: Inlined)(implicit ctx: Context): Tree = {
    if (enclosingInlineds.nonEmpty) inlined // Remove in the outer most inlined call
    else {
      val inlinedAtPos = inlined.call.pos
      val callSourceFile = ctx.source.file

      /** Removes all Inlined trees, replacing them with blocks.
       *  Repositions all trees directly inside an inlined expansion of a non empty call to the position of the call.
       *  Any tree directly inside an empty call (inlined in the inlined code) retains their position.
       */
      class Reposition extends TreeMap {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = {
          tree match {
            case tree: Inlined => transformInline(tree)
            case _ =>
              val transformed = super.transform(tree)
              enclosingInlineds match {
                case call :: _ if call.symbol.sourceFile != callSourceFile =>
                  // Until we implement JSR-45, we cannot represent in output positions in other source files.
                  // So, reposition inlined code from other files with the call position:
                  transformed.withPos(inlinedAtPos)
                case _ => transformed
              }
          }
        }
        def transformInline(tree: Inlined)(implicit ctx: Context): Tree = {
          tpd.seq(transformSub(tree.bindings), transform(tree.expansion)(inlineContext(tree.call)))
        }
      }

      (new Reposition).transformInline(inlined)
    }
  }

  /** Leave only a call trace consisting of
   *  - a reference to the top-level class from which the call was inlined,
   *  - the call's position
   *  in the call field of an Inlined node.
   *  The trace has enough info to completely reconstruct positions.
   */
  def inlineCallTrace(callSym: Symbol, pos: Position)(implicit ctx: Context): Tree =
    Ident(callSym.topLevelClass.typeRef).withPos(pos)
}

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call         the original call to an inlineable method
 *  @param  rhsToInline  the body of the inlineable method that replaces the call.
 */
class Inliner(call: tpd.Tree, rhsToInline: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private val (methPart, callTypeArgs, callValueArgss) = decomposeCall(call)
  private val inlinedMethod = methPart.symbol
  private val inlineCallPrefix = qualifier(methPart)

  // Make sure all type arguments to the call are fully determined
  for (targ <- callTypeArgs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

  /** A map from parameter names of the inlineable method to references of the actual arguments.
   *  For a type argument this is the full argument type.
   *  For a value argument, it is a reference to either the argument value
   *  (if the argument is a pure expression of singleton type), or to `val` or `def` acting
   *  as a proxy (if the argument is something else).
   */
  private val paramBinding = new mutable.HashMap[Name, Type]

  /** A map from references to (type and value) parameters of the inlineable method
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

  /** A binding for the parameter of an inline method. This is a `val` def for
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
    var inlineFlag = InlineProxy
    if (paramtp.hasAnnotation(defn.InlineParamAnnot)) inlineFlag |= Inline
    val (bindingFlags, bindingType) =
      if (isByName) (Method | InlineProxy, ExprType(argtpe.widen))
      else (inlineFlag, argtpe.widen)
    val boundSym = newSym(name, bindingFlags, bindingType).asTerm
    val binding = {
      if (isByName) DefDef(boundSym, arg.changeOwner(ctx.owner, boundSym))
      else ValDef(boundSym, arg)
    }.withPos(boundSym.pos)
    boundSym.defTree = binding
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
      assert(argss.nonEmpty, i"missing bindings: $tp in $call")
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
      val binding = ValDef(selfSym.asTerm, rhs).withPos(selfSym.pos)
      bindingsBuf += binding
      selfSym.defTree = binding
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
   *      inline method, create a proxy symbol and bind the thistype to refer to the proxy.
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
      def adaptToPrefix(tp: Type) = tp.asSeenFrom(inlineCallPrefix.tpe, inlinedMethod.owner)
      val proxyType = inlineCallPrefix.tpe.dealias.tryNormalize match {
        case typeMatchResult if typeMatchResult.exists => typeMatchResult
        case _ => adaptToPrefix(tpe).widenIfUnstable
      }
      thisProxy(tpe.cls) = newSym(proxyName, InlineProxy, proxyType).termRef
      if (!tpe.cls.isStaticOwner)
        registerType(inlinedMethod.owner.thisType) // make sure we have a base from which to outer-select
      for (param <- tpe.cls.typeParams)
        paramProxy(param.typeRef) = adaptToPrefix(param.typeRef)
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
  def integrate(tree: Tree, originalOwner: Symbol)(implicit ctx: Context): Tree = {
    val result = tree.changeOwner(originalOwner, ctx.owner)
    if (!originalOwner.isContainedIn(inlinedMethod)) Inlined(EmptyTree, Nil, result)
    else result
  }

  def tryConstValue: Tree =
    ctx.typeComparer.constValue(callTypeArgs.head.tpe) match {
      case Some(c) => Literal(c).withPos(call.pos)
      case _ => EmptyTree
    }

  /** The Inlined node representing the inlined call */
  def inlined(pt: Type): Tree = {

    if (callTypeArgs.length == 1)
      if (inlinedMethod == defn.Typelevel_constValue) {
        val constVal = tryConstValue
        if (!constVal.isEmpty) return constVal
        ctx.error(i"not a constant type: ${callTypeArgs.head}; cannot take constValue", call.pos)
      }
      else if (inlinedMethod == defn.Typelevel_constValueOpt) {
        val constVal = tryConstValue
        return (
          if (constVal.isEmpty) ref(defn.NoneModuleRef)
          else New(defn.SomeClass.typeRef.appliedTo(constVal.tpe), constVal :: Nil)
        )
      }

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
            case Some(t) if tree.isTerm && t.isSingleton => singleton(t.dealias).withPos(tree.pos)
            case Some(t) if tree.isType => TypeTree(t).withPos(tree.pos)
            case _ => tree
          }
        case tree => tree
      },
      oldOwners = inlinedMethod :: Nil,
      newOwners = ctx.owner :: Nil
    )(inlineCtx)

    // Apply inliner to `rhsToInline`, split off any implicit bindings from result, and
    // make them part of `bindingsBuf`. The expansion is then the tree that remains.
    val expansion = inliner.transform(rhsToInline.withPos(call.pos))

    def issueError() = callValueArgss match {
      case (msgArg :: rest) :: Nil =>
        msgArg.tpe match {
          case ConstantType(Constant(msg: String)) =>
            // Usually `error` is called from within a rewrite method. In this
            // case we need to report the error at the point of the outermost enclosing inline
            // call. This way, a defensively written rewrite methid can always
            // report bad inputs at the point of call instead of revealing its internals.
            val callToReport = if (enclosingInlineds.nonEmpty) enclosingInlineds.last else call
            val ctxToReport = ctx.outersIterator.dropWhile(enclosingInlineds(_).nonEmpty).next
            def issueInCtx(implicit ctx: Context) = {
              def decompose(arg: Tree): String = arg match {
                case Typed(arg, _) => decompose(arg)
                case SeqLiteral(elems, _) => elems.map(decompose).mkString(", ")
                case arg =>
                  arg.tpe.widenTermRefExpr match {
                    case ConstantType(Constant(c)) => c.toString
                    case _ => arg.show
                  }
              }
              ctx.error(s"$msg${rest.map(decompose).mkString(", ")}", callToReport.pos)
            }
            issueInCtx(ctxToReport)
          case _ =>
        }
      case _ =>
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
      val (finalBindings, finalExpansion) = dropUnusedDefs(bindingsBuf.toList, expansion1)

      if (inlinedMethod == defn.Typelevel_error) issueError()

      // Take care that only argument bindings go into `bindings`, since positions are
      // different for bindings from arguments and bindings from body.
      tpd.Inlined(call, finalBindings, finalExpansion)
    }
  }

  /** A utility object offering methods for rewriting inlined code */
  object reducer {

    /** An extractor for terms equivalent to `new C(args)`, returning the class `C`,
     *  a list of bindings, and the arguments `args`. Can see inside blocks and Inlined nodes and can
     *  follow a reference to an inline value binding to its right hand side.
     *  @return    optionally, a triple consisting of
     *             - the class `C`
     *             - the arguments `args`
     *             - any bindings that wrap the instance creation
     *             - whether the instance creation is precomputed or by-name
     */
    private object NewInstance {
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Symbol, List[Tree], List[Tree], Boolean)] = {
        def unapplyLet(bindings: List[Tree], expr: Tree) =
          unapply(expr) map {
            case (cls, reduced, prefix, precomputed) => (cls, reduced, bindings ::: prefix, precomputed)
          }
        tree match {
          case Apply(fn, args) =>
            fn match {
              case Select(New(tpt), nme.CONSTRUCTOR) =>
                Some((tpt.tpe.classSymbol, args, Nil, false))
              case TypeApply(Select(New(tpt), nme.CONSTRUCTOR), _) =>
                Some((tpt.tpe.classSymbol, args, Nil, false))
              case _ =>
                val meth = fn.symbol
                if (meth.name == nme.apply &&
                    meth.flags.is(Synthetic) &&
                    meth.owner.linkedClass.is(Case))
                  Some(meth.owner.linkedClass, args, Nil, false)
                else None
            }
          case Ident(_) =>
            val binding = tree.symbol.defTree
            for ((cls, reduced, prefix, precomputed) <- unapply(binding))
            yield (cls, reduced, prefix, precomputed || binding.isInstanceOf[ValDef])
          case Inlined(_, bindings, expansion) =>
            unapplyLet(bindings, expansion)
          case Block(stats, expr) if isPureExpr(tree) =>
            unapplyLet(stats, expr)
          case _ =>
            None
        }
      }
    }

    /** If `tree` is equivalent to `new C(args).x` where class `C` does not have
     *  initialization code and `x` is a parameter corresponding to one of the
     *  arguments `args`, the corresponding argument, otherwise `tree` itself.
     *  Side effects of original arguments need to be preserved.
     */
    def reduceProjection(tree: Tree)(implicit ctx: Context): Tree = {
      if (ctx.debug) inlining.println(i"try reduce projection $tree")
      tree match {
        case Select(NewInstance(cls, args, prefix, precomputed), field) if cls.isNoInitsClass =>
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
            def finish(arg: Tree) =
              new TreeTypeMap().transform(arg) // make sure local bindings in argument have fresh symbols
                .reporting(res => i"projecting $tree -> $res", inlining)
            val arg = args(idx)
            if (precomputed)
              if (isPureExpr(arg)) finish(arg)
              else tree // nothing we can do here, projection would duplicate side effect
            else {
              // newInstance is evaluated in place, need to reflect side effects of
              // arguments in the order they were written originally
              def collectImpure(from: Int, end: Int) =
                (from until end).filterNot(i => isPureExpr(args(i))).toList.map(args)
              val leading = collectImpure(0, idx)
              val trailing = collectImpure(idx + 1, args.length)
              val argInPlace =
                if (trailing.isEmpty) arg
                else letBindUnless(TreeInfo.Pure, arg)(seq(trailing, _))
              finish(seq(prefix, seq(leading, argInPlace)))
            }
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
          binding.symbol.defTree = rhs1
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
          search(bindingsBuf) match {
            case Some(vdef: ValDef) if vdef.symbol.is(Inline) =>
              Some(integrate(vdef.rhs, vdef.symbol))
            case Some(ddef: DefDef) =>
              Some(integrate(ddef.rhs, ddef.symbol))
            case _ => None
          }
        else None
      }
    }

    object ConstantValue {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Any] = tree.tpe.widenTermRefExpr.normalized match {
        case ConstantType(Constant(x)) => Some(x)
        case _ => None
      }
    }

    def tryInline(tree: Tree)(implicit ctx: Context): Tree = tree match {
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
    def betaReduce(tree: Tree)(implicit ctx: Context): Tree = tree match {
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
            seq(bindingsBuf.toList, expander.transform(ddef.rhs))
          case _ => tree
        }
      case _ => tree
    }

    /** The result type of reducing a match. It consists optionally of a list of bindings
     *  for the pattern-bound variables and the RHS of the selected case.
     *  Returns `None` if no case was selected.
     */
    type MatchRedux = Option[(List[MemberDef], tpd.Tree)]

    /** Reduce an inline match
     *   @param     mtch          the match tree
     *   @param     scrutinee     the scrutinee expression, assumed to be pure, or
     *                            EmptyTree for an implicit match
     *   @param     scrutType     its fully defined type, or
     *                            ImplicitScrutineeTypeRef for an implicit match
     *   @param     typer         The current inline typer
     *   @return    optionally, if match can be reduced to a matching case: A pair of
     *              bindings for all pattern-bound variables and the RHS of the case.
     */
    def reduceInlineMatch(scrutinee: Tree, scrutType: Type, cases: List[CaseDef], typer: Typer)(implicit ctx: Context): MatchRedux = {

      val isImplicit = scrutinee.isEmpty
      val gadtSyms = typer.gadtSyms(scrutType)

      /** Try to match pattern `pat` against scrutinee reference `scrut`. If successful add
       *  bindings for variables bound in this pattern to `bindingsBuf`.
       */
      def reducePattern(bindingsBuf: mutable.ListBuffer[MemberDef], scrut: TermRef, pat: Tree)(implicit ctx: Context): Boolean = {

      	/** Create a binding of a pattern bound variable with matching part of
      	 *  scrutinee as RHS and type that corresponds to RHS.
      	 */
        def newBinding(sym: TermSymbol, rhs: Tree): Unit = {
          sym.info = rhs.tpe.widenTermRefExpr
          bindingsBuf += ValDef(sym, constToLiteral(rhs)).withPos(sym.pos)
        }

        def searchImplicit(sym: TermSymbol, tpt: Tree) = {
          val evTyper = new Typer
          val evidence = evTyper.inferImplicitArg(tpt.tpe, tpt.pos)(ctx.fresh.setTyper(evTyper))
          evidence.tpe match {
            case fail: Implicits.AmbiguousImplicits =>
              ctx.error(evTyper.missingArgMsg(evidence, tpt.tpe, ""), tpt.pos)
              true // hard error: return true to stop implicit search here
            case fail: Implicits.SearchFailureType =>
              false
            case _ =>
              newBinding(sym, evidence)
              true
          }
        }

        pat match {
          case Typed(pat1, tpt) =>
            val getBoundVars = new TreeAccumulator[List[TypeSymbol]] {
              def apply(syms: List[TypeSymbol], t: Tree)(implicit ctx: Context) = {
                val syms1 = t match {
                  case t: Bind if t.symbol.isType =>
                    t.symbol.asType :: syms
                  case _ =>
                    syms
                }
                foldOver(syms1, t)
              }
            }
            var boundVars = getBoundVars(Nil, tpt)
            // UnApply nodes with pattern bound variables translate to something like this
            //   UnApply[t @ t](pats)(implicits): T[t]
            // Need to traverse any binds in type arguments of the UnAppyl to get the set of
            // all instantiable type variables. Test case is pos/inline-caseclass.scala.
            pat1 match {
              case UnApply(TypeApply(_, tpts), _, _) =>
                for (tpt <- tpts) boundVars = getBoundVars(boundVars, tpt)
              case _ =>
            }
            for (bv <- boundVars) {
              val TypeBounds(lo, hi) = bv.info.bounds
              ctx.gadt.addBound(bv, lo, isUpper = false)
              ctx.gadt.addBound(bv, hi, isUpper = true)
            }
            scrut <:< tpt.tpe && {
              for (bv <- boundVars) {
                bv.info = TypeAlias(ctx.gadt.bounds(bv).lo)
                  // FIXME: This is very crude. We should approximate with lower or higher bound depending
                  // on variance, and we should also take care of recursive bounds. Basically what
                  // ConstraintHandler#approximation does. However, this only works for constrained paramrefs
                  // not GADT-bound variables. Hopefully we will get some way to improve this when we
                  // re-implement GADTs in terms of constraints.
                if (bv.name != nme.WILDCARD) bindingsBuf += TypeDef(bv)
              }
              reducePattern(bindingsBuf, scrut, pat1)
            }
          case pat @ Bind(name: TermName, Typed(_, tpt)) if isImplicit =>
            searchImplicit(pat.symbol.asTerm, tpt)
          case pat @ Bind(name: TermName, body) =>
            reducePattern(bindingsBuf, scrut, body) && {
              if (name != nme.WILDCARD) newBinding(pat.symbol.asTerm, ref(scrut))
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

                def reduceSubPatterns(pats: List[Tree], selectors: List[Tree]): Boolean = (pats, selectors) match {
                  case (Nil, Nil) => true
                  case (pat :: pats1, selector :: selectors1) =>
                    val elem = newSym(InlineBinderName.fresh(), Synthetic, selector.tpe.widenTermRefExpr).asTerm
                    newBinding(elem, selector)
                    reducePattern(bindingsBuf, elem.termRef, pat) &&
                    reduceSubPatterns(pats1, selectors1)
                  case _ => false
                }

                val paramType = mt.paramInfos.head
                val paramCls = paramType.classSymbol
                if (paramCls.is(Case) && unapp.symbol.is(Synthetic) && scrut <:< paramType) {
                  val caseAccessors =
                    if (paramCls.is(Scala2x)) paramCls.caseAccessors.filter(_.is(Method))
                    else paramCls.asClass.paramAccessors
                  val selectors =
                    for (accessor <- caseAccessors)
                    yield constToLiteral(reduceProjection(ref(scrut).select(accessor).ensureApplied))
                  caseAccessors.length == pats.length && reduceSubPatterns(pats, selectors)
                }
                else if (unapp.symbol.isInlineMethod) { // TODO: Adapt to typed setting
                  val app = untpd.Apply(untpd.TypedSplice(unapp), untpd.ref(scrut))
                  val app1 = typer.typedExpr(app)
                  val args = tupleArgs(app1)
                  args.nonEmpty && reduceSubPatterns(pats, args)
                }
                else false
              case _ =>
                false
            }
          case _ => false
        }
      }

      /** The initial scrutinee binding: `val $scrutineeN = <scrutinee>` */
      val scrutineeSym = newSym(InlineScrutineeName.fresh(), Synthetic, scrutType).asTerm
      val scrutineeBinding = normalizeBinding(ValDef(scrutineeSym, scrutinee))

      def reduceCase(cdef: CaseDef): MatchRedux = {
        val caseBindingsBuf = new mutable.ListBuffer[MemberDef]()
        def guardOK(implicit ctx: Context) = cdef.guard.isEmpty || {
          val guardCtx = ctx.fresh.setNewScope
          caseBindingsBuf.foreach(binding => guardCtx.enter(binding.symbol))
          typer.typed(cdef.guard, defn.BooleanType)(guardCtx) match {
            case ConstantValue(true) => true
            case _ => false
          }
        }
        if (!isImplicit) caseBindingsBuf += scrutineeBinding
        val gadtCtx = typer.gadtContext(gadtSyms).addMode(Mode.GADTflexible)
        val pat1 = typer.typedPattern(cdef.pat, scrutType)(gadtCtx)
        if (reducePattern(caseBindingsBuf, scrutineeSym.termRef, pat1)(gadtCtx) && guardOK)
          Some((caseBindingsBuf.toList, cdef.body))
        else
          None
      }

      def recur(cases: List[CaseDef]): MatchRedux = cases match {
        case Nil => None
        case cdef :: cases1 => reduceCase(cdef) `orElse` recur(cases1)
      }

      recur(cases)
    }
  }

  /** A typer for inlined bodies. Beyond standard typing, an inline typer performs
   *  the following functions:
   *
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are by-name closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  class InlineTyper extends ReTyper {
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

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree =
      tryInline(tree.asInstanceOf[tpd.Tree]) `orElse` super.typedIdent(tree, pt)

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.hasType, tree)
      val qual1 = typed(tree.qualifier, selectionProto(tree.name, pt, this))
      val res = untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
      ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.pos)
      res
    }

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context): Tree =
      typed(tree.cond, defn.BooleanType) match {
        case cond1 @ ConstantValue(b: Boolean) =>
          val selected0 = if (b) tree.thenp else tree.elsep
          val selected = if (selected0.isEmpty) tpd.Literal(Constant(())) else typed(selected0, pt)
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case cond1 =>
          if (tree.isInline)
            errorTree(tree, em"""cannot reduce inline if
                                | its condition   ${tree.cond}
                                | is not a constant value""")
          else {
            val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
            super.typedIf(if1, pt)
          }
      }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree =
      constToLiteral(betaReduce(super.typedApply(tree, pt)))

    override def typedMatchFinish(tree: untpd.Match, sel: Tree, wideSelType: Type, cases: List[untpd.CaseDef], pt: Type)(implicit ctx: Context) =
      if (!tree.isInline || ctx.owner.isInlineMethod) // don't reduce match of nested inline method yet
        super.typedMatchFinish(tree, sel, wideSelType, cases, pt)
      else {
        val selType = if (sel.isEmpty) wideSelType else sel.tpe
        reduceInlineMatch(sel, selType, cases.asInstanceOf[List[CaseDef]], this) match {
          case Some((caseBindings, rhs0)) =>
            val (usedBindings, rhs1) = dropUnusedDefs(caseBindings, rhs0)
            val rhs = seq(usedBindings, rhs1)
            inlining.println(i"""--- reduce:
                                |$tree
                                |--- to:
                                |$rhs""")
            typedExpr(rhs, pt)
          case None =>
            def guardStr(guard: untpd.Tree) = if (guard.isEmpty) "" else i" if $guard"
            def patStr(cdef: untpd.CaseDef) = i"case ${cdef.pat}${guardStr(cdef.guard)}"
            val msg =
              if (tree.selector.isEmpty)
                em"""cannot reduce implicit match with
                   | patterns :  ${tree.cases.map(patStr).mkString("\n             ")}"""
              else
                em"""cannot reduce inline match with
                    | scrutinee:  $sel : ${selType}
                    | patterns :  ${tree.cases.map(patStr).mkString("\n             ")}"""
            errorTree(tree, msg)
        }
      }

    override def newLikeThis: Typer = new InlineTyper
  }

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  def dropUnusedDefs(bindings: List[MemberDef], tree: Tree)(implicit ctx: Context): (List[MemberDef], Tree) = {
    // inlining.println(i"drop unused $bindings%, % in $tree")
    val refCount = newMutableSymbolMap[Int]
    val bindingOfSym = newMutableSymbolMap[MemberDef]
    val dealiased = new java.util.IdentityHashMap[Type, Type]()

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
    } && !boundSym.is(ImplicitInlineMethod)

    val (termBindings, typeBindings) = bindings.partition(_.symbol.isTerm)

    /** drop any referenced type symbols from the given set of type symbols */
    val dealiasTypeBindings = new TreeMap {
      val boundTypes = typeBindings.map(_.symbol).toSet

      val dealias = new TypeMap {
        override def apply(tp: Type) = dealiased.get(tp) match {
          case null =>
            val tp1 = mapOver {
              tp match {
                case tp: TypeRef if boundTypes.contains(tp.symbol) =>
                  val TypeAlias(alias) = tp.info
                  alias
                case _ => tp
              }
            }
            dealiased.put(tp, tp1)
            tp1
          case tp1 => tp1
        }
      }

      override def transform(t: Tree)(implicit ctx: Context) = {
        val dealiasedType = dealias(t.tpe)
        val t1 = t match {
          case t: RefTree =>
            if (t.name != nme.WILDCARD && boundTypes.contains(t.symbol)) TypeTree(dealiasedType).withPos(t.pos)
            else t.withType(dealiasedType)
          case t: DefTree =>
            t.symbol.info = dealias(t.symbol.info)
            t
          case _ =>
            t.withType(dealiasedType)
        }
        super.transform(t1)
      }
    }

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
        case Block(Nil, expr) =>
          super.transform(expr)
        case _ =>
          super.transform(t)
      }
    }

    val dealiasedTermBindings =
      termBindings.mapconserve(dealiasTypeBindings.transform).asInstanceOf[List[MemberDef]]
    val dealiasedTree = dealiasTypeBindings.transform(tree)

    val retained = dealiasedTermBindings.filterConserve(binding => retain(binding.symbol))
    if (retained `eq` dealiasedTermBindings) {
      (dealiasedTermBindings, dealiasedTree)
    }
    else {
      val expanded = inlineBindings.transform(dealiasedTree)
      dropUnusedDefs(retained, expanded)
    }
  }
}
