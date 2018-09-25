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
import StdNames._
import Contexts.Context
import Names.{Name, TermName, EmptyTermName}
import NameOps._
import NameKinds.{ClassifiedNameKind, InlineAccessorName, UniqueInlineName, InlineScrutineeName, InlineBinderName}
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

  val typedInline = true

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
    case _ => isInlineable(tree.symbol)
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
  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
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
  private val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]

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
                              bindingsBuf: mutable.ListBuffer[ValOrDefDef]): ValOrDefDef = {
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
      val proxyType = tpe.asSeenFrom(inlineCallPrefix.tpe, inlinedMethod.owner)
      thisProxy(tpe.cls) = newSym(proxyName, InlineProxy, proxyType).termRef
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

    /** An extractor for references to inlineable arguments. These are :
     *   - by-value arguments marked with `inline`
     *   - all by-name arguments
     */
    private object InlineableArg {
      lazy val paramProxies = paramProxy.values.toSet
      def unapply(tree: Trees.Ident[_])(implicit ctx: Context): Option[Tree] = {
        def search(buf: mutable.ListBuffer[ValOrDefDef]) = buf.find(_.name == tree.name)
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
            seq(bindingsBuf.toList, expander.transform(ddef.rhs))
          case _ => tree
        }
      case _ => tree
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


    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context) =
      tryInline(tree.asInstanceOf[tpd.Tree]) `orElse` super.typedIdent(tree, pt)

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.hasType, tree)
      val qual1 = typed(tree.qualifier, selectionProto(tree.name, pt, this))
      val res = untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
      ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.pos)
      res
    }

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

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) =
      constToLiteral(betaReduce(super.typedApply(tree, pt)))

    override def newLikeThis: Typer = new InlineTyper
  }

  /** Drop any side-effect-free bindings that are unused in expansion or other reachable bindings.
   *  Inline def bindings that are used only once.
   */
  def dropUnusedDefs(bindings: List[ValOrDefDef], tree: Tree)(implicit ctx: Context): (List[ValOrDefDef], Tree) = {
    val refCount = newMutableSymbolMap[Int]
    val bindingOfSym = newMutableSymbolMap[ValOrDefDef]
    val dealiased = new java.util.IdentityHashMap[Type, Type]()

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
            if (boundTypes.contains(t.symbol)) TypeTree(dealiasedType).withPos(t.pos)
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
      termBindings.mapconserve(dealiasTypeBindings.transform).asInstanceOf[List[ValOrDefDef]]
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
