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

object Inliner {
  import tpd._

  /** Marks an implicit reference found in the context (as opposed to the implicit scope)
   *  from an inlineable body. Such references will be carried along with the body to
   *  the expansion site.
   */
  private val ContextualImplicit = new Property.StickyKey[Unit]

  def markContextualImplicit(tree: Tree)(implicit ctx: Context): Unit =
    methPart(tree).putAttachment(ContextualImplicit, ())

  class InlineAccessors extends AccessProxies {

    /** If an inline accessor name wraps a unique inline name, this is taken as indication
     *  that the inline accessor takes its receiver as first parameter. Such accessors
     *  are created by MakeInlineablePassing.
     */
    override def passReceiverAsArg(name: Name)(implicit ctx: Context) = name match {
      case InlineAccessorName(UniqueInlineName(_, _)) => true
      case _ => false
    }

    /** A tree map which inserts accessors for non-public term members accessed from inlined code.
     */
    abstract class MakeInlineableMap(val inlineSym: Symbol) extends TreeMap with Insert {
      def accessorNameKind = InlineAccessorName

      /** A definition needs an accessor if it is private, protected, or qualified private
       *  and it is not part of the tree that gets inlined. The latter test is implemented
       *  by excluding all symbols properly contained in the inlined method.
       *
       *  Constant vals don't need accessors since they are inlined in FirstTransform.
       */
      def needsAccessor(sym: Symbol)(implicit ctx: Context) =
        sym.isTerm &&
        (sym.is(AccessFlags) || sym.privateWithin.exists) &&
        !sym.isContainedIn(inlineSym) &&
        !(sym.isStable && sym.info.widenTermRefExpr.isInstanceOf[ConstantType])

      def preTransform(tree: Tree)(implicit ctx: Context): Tree

      def postTransform(tree: Tree)(implicit ctx: Context) = tree match {
        case Assign(lhs, rhs) if lhs.symbol.name.is(InlineAccessorName) =>
          val setter = useSetter(lhs)
          if (inlineSym.isTransparentMethod) tree // just generate a setter, but don't integrate it in the tree
          else cpy.Apply(tree)(setter, rhs :: Nil)
        case _ =>
          tree
      }

      override def transform(tree: Tree)(implicit ctx: Context): Tree =
        postTransform(super.transform(preTransform(tree)))
    }

    /** Direct approach: place the accessor with the accessed symbol. This has the
     *  advantage that we can re-use the receiver as is. But it is only
     *  possible if the receiver is essentially this or an outer this, which is indicated
     *  by the test that we can find a host for the accessor.
     */
    class MakeInlineableDirect(inlineSym: Symbol) extends MakeInlineableMap(inlineSym) {
      def preTransform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: RefTree if needsAccessor(tree.symbol) =>
          if (tree.symbol.isConstructor) {
            ctx.error("Implementation restriction: cannot use private constructors in inline methods", tree.pos)
            tree // TODO: create a proper accessor for the private constructor
          }
          else useAccessor(tree)
        case _ =>
          tree
      }
      override def ifNoHost(reference: RefTree)(implicit ctx: Context): Tree = reference
    }

    /** Fallback approach if the direct approach does not work: Place the accessor method
     *  in the same class as the inlined method, and let it take the receiver as parameter.
     *  This is tricky, since we have to find a suitable type for the parameter, which might
     *  require additional type parameters for the inline accessor. An example is in the
     *  `TestPassing` class in test `run/inline/inlines_1`:
     *
     *    class C[T](x: T) {
     *      private[inlines] def next[U](y: U): (T, U) = (x, y)
     *    }
     *    class TestPassing {
     *      inline def foo[A](x: A): (A, Int) = {
     *      val c = new C[A](x)
     *      c.next(1)
     *    }
     *    inline def bar[A](x: A): (A, String) = {
     *      val c = new C[A](x)
     *      c.next("")
     *    }
     *
     *  `C` could be compiled separately, so we cannot place the inline accessor in it.
     *  Instead, the inline accessor goes into `TestPassing` and takes the actual receiver
     *  type as argument:
     *
     *    def inline$next$i1[A, U](x$0: C[A])(y: U): (A, U) =
     *      x$0.next[U](y)
     *
     *  Since different calls might have different receiver types, we need to generate one
     *  such accessor per call, so they need to have unique names.
     */
    class MakeInlineablePassing(inlineSym: Symbol) extends MakeInlineableMap(inlineSym) {

      def preTransform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case _: Apply | _: TypeApply | _: RefTree
        if needsAccessor(tree.symbol) && tree.isTerm && !tree.symbol.isConstructor =>
          val (refPart, targs, argss) = decomposeCall(tree)
          val qual = qualifier(refPart)
          inlining.println(i"adding receiver passing inline accessor for $tree/$refPart -> (${qual.tpe}, $refPart: ${refPart.getClass}, [$targs%, %], ($argss%, %))")

          // Need to dealias in order to cagtch all possible references to abstracted over types in
          // substitutions
          val dealiasMap = new TypeMap {
            def apply(t: Type) = mapOver(t.dealias)
          }
          val qualType = dealiasMap(qual.tpe.widen)

          // The types that are local to the inlined method, and that therefore have
          // to be abstracted out in the accessor, which is external to the inlined method
          val localRefs = qualType.namedPartsWith(ref =>
            ref.isType && ref.symbol.isContainedIn(inlineSym)).toList

          // Add qualifier type as leading method argument to argument `tp`
          def addQualType(tp: Type): Type = tp match {
            case tp: PolyType => tp.derivedLambdaType(tp.paramNames, tp.paramInfos, addQualType(tp.resultType))
            case tp: ExprType => addQualType(tp.resultType)
            case tp => MethodType(qualType.simplified :: Nil, tp)
          }

          // Abstract accessed type over local refs
          def abstractQualType(mtpe: Type): Type =
            if (localRefs.isEmpty) mtpe
            else PolyType.fromParams(localRefs.map(_.symbol.asType), mtpe)
              .asInstanceOf[PolyType].flatten

          val accessed = refPart.symbol.asTerm
          val accessedType = refPart.tpe.widen
          val accessor = accessorSymbol(
            owner = inlineSym.owner,
            accessorName = InlineAccessorName(UniqueInlineName.fresh(accessed.name)),
            accessorInfo = abstractQualType(addQualType(dealiasMap(accessedType))),
            accessed = accessed)

          ref(accessor)
            .appliedToTypeTrees(localRefs.map(TypeTree(_)) ++ targs)
            .appliedToArgss((qual :: Nil) :: argss)
            .withPos(tree.pos)

            // TODO: Handle references to non-public types.
            // This is quite tricky, as such types can appear anywhere, including as parts
            // of types of other things. For the moment we do nothing and complain
            // at the implicit expansion site if there's a reference to an inaccessible type.
            // Draft code (incomplete):
            //
            //  val accessor = accessorSymbol(tree, TypeAlias(tree.tpe)).asType
            //  myAccessors += TypeDef(accessor).withPos(tree.pos.focus)
            //  ref(accessor).withPos(tree.pos)
            //
        case _ => tree
      }
    }

    /** Adds accessors for all non-public term members accessed
    *  from `tree`. Non-public type members are currently left as they are.
    *  This means that references to a private type will lead to typing failures
    *  on the code when it is inlined. Less than ideal, but hard to do better (see below).
    *
    *  @return If there are accessors generated, a thicket consisting of the rewritten `tree`
    *          and all accessors, otherwise the original tree.
    */
    def makeInlineable(tree: Tree)(implicit ctx: Context) = {
      val inlineSym = ctx.owner
      if (inlineSym.owner.isTerm)
        // Inline methods in local scopes can only be called in the scope they are defined,
        // so no accessors are needed for them.
        tree
      else
        new MakeInlineablePassing(inlineSym).transform(
          new MakeInlineableDirect(inlineSym).transform(tree))
    }
  }

  def isLocal(sym: Symbol, inlineMethod: Symbol)(implicit ctx: Context) =
    sym.isContainedIn(inlineMethod) &&
    sym != inlineMethod &&
    (!sym.is(Param) || sym.owner != inlineMethod)

  /** Register inline info for given inline method `sym`.
   *
   *  @param sym         The symbol denotatioon of the inline method for which info is registered
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param ctx         The context to use for evaluating `treeExpr`. It needs
   *                     to have the inlined method as owner.
   */
  def registerInlineInfo(
      inlined: Symbol, originalBody: untpd.Tree, treeExpr: Context => Tree)(implicit ctx: Context): Unit = {
    inlined.unforcedAnnotation(defn.BodyAnnot) match {
      case Some(ann: ConcreteBodyAnnotation) =>
      case Some(ann: LazyBodyAnnotation) if ann.isEvaluated =>
      case _ =>
        if (!ctx.isAfterTyper) {
          val inlineCtx = ctx
          inlined.updateAnnotation(LazyBodyAnnotation { _ =>
            implicit val ctx = inlineCtx
            val rawBody = treeExpr(ctx)
            val typedBody =
              if (ctx.reporter.hasErrors) rawBody
              else ctx.compilationUnit.inlineAccessors.makeInlineable(rawBody)
            val inlineableBody =
              if (inlined.isInlinedMethod) typedBody
              else addReferences(inlined, originalBody, typedBody)
            inlining.println(i"Body to inline for $inlined: $inlineableBody")
            inlineableBody
          })
        }
    }
  }

  /** Tweak untyped tree `original` so that all external references are typed
   *  and it reflects the changes in the corresponding typed tree `typed` that
   *  make `typed` inlineable. Concretely:
   *
   *   - all external references via identifiers or this-references are converted
   *     to typed splices,
   *   - if X gets an inline accessor in `typed`, references to X in `original`
   *     are converted to the inline accessor name.
   */
  def addReferences(inlineMethod: Symbol,
      original: untpd.Tree, typed: tpd.Tree)(implicit ctx: Context): tpd.Tree = {

    def isExternal(sym: Symbol) = sym.exists && !isLocal(sym, inlineMethod)

    // Maps from positions to external reference types and inline selector names.
    object referenced extends TreeTraverser {
      val typeAtPos = mutable.Map[Position, Type]()
      val nameAtPos = mutable.Map[Position, Name]()
      val implicitSyms = mutable.Set[Symbol]()
      val implicitRefs = new mutable.ListBuffer[Tree]
      def registerIfContextualImplicit(tree: Tree) = tree match {
        case tree: RefTree
        if tree.removeAttachment(ContextualImplicit).isDefined &&
           isExternal(tree.symbol) &&
           !implicitSyms.contains(tree.symbol) =>
          if (tree.existsSubTree(t => isLocal(tree.symbol, inlineMethod)))
            ctx.warning("implicit reference $tree is dropped at inline site because it refers to local symbol(s)", tree.pos)
          else {
            implicitSyms += tree.symbol
            implicitRefs += tree
          }
        case _ =>
      }
      def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case _: Ident | _: This =>
            //println(i"leaf: $tree at ${tree.pos}")
            if (isExternal(tree.symbol)) {
              inlining.println(i"type at pos ${tree.pos.toSynthetic} = ${tree.tpe}")
              typeAtPos(tree.pos.toSynthetic) = tree.tpe
            }
          case _: Select if tree.symbol.name.is(InlineAccessorName) =>
            inlining.println(i"accessor: $tree at ${tree.pos}")
            nameAtPos(tree.pos.toSynthetic) = tree.symbol.name
          case _ =>
        }
        registerIfContextualImplicit(tree)
        traverseChildren(tree)
      }
    }
    referenced.traverse(typed)

    // The untyped tree transform that applies the tweaks
    object addRefs extends untpd.UntypedTreeMap {
      override def transform(tree: untpd.Tree)(implicit ctx: Context): untpd.Tree = {
        def adjustLeaf(tree: untpd.Tree): untpd.Tree = referenced.typeAtPos.get(tree.pos.toSynthetic) match {
          case Some(tpe) => untpd.TypedSplice(tree.withType(tpe))
          case none => tree
        }
        def adjustName(name: Name) = referenced.nameAtPos.get(tree.pos.toSynthetic) match {
          case Some(n) => n
          case none => name
        }
        def adjustQualifier(tree: untpd.Tree): untpd.Tree = tree match {
          case tree @ Ident(name1) =>
            referenced.typeAtPos.get(tree.pos.startPos) match {
              case Some(tp: ThisType) =>
                val qual = untpd.TypedSplice(This(tp.cls).withPos(tree.pos.startPos))
                cpy.Select(tree)(qual, name1)
              case none =>
                tree
            }
          case tree => tree
        }
        val tree1 = super.transform(tree)
        tree1 match {
          case This(_) =>
            adjustLeaf(tree1)
          case Ident(name) =>
            adjustQualifier(adjustLeaf(cpy.Ident(tree1)(adjustName(name))))
          case Select(pre, name) =>
            cpy.Select(tree1)(pre, adjustName(name))
          case tree: untpd.DerivedTypeTree =>
            inlining.println(i"inlining derived $tree --> ${ctx.typer.typed(tree)}")
            untpd.TypedSplice(ctx.typer.typed(tree))
          case _ =>
            tree1
        }
      }
    }
    val implicitBindings =
      for (iref <- referenced.implicitRefs.toList) yield {
        val localImplicit = iref.symbol.asTerm.copy(
          owner = inlineMethod,
          name = UniqueInlineName.fresh(iref.symbol.name.asTermName),
          flags = Implicit | Method | Stable,
          info = iref.symbol.info.ensureMethodic,
          coord = inlineMethod.pos).asTerm
        polyDefDef(localImplicit, tps => vrefss =>
            iref.appliedToTypes(tps).appliedToArgss(vrefss))
      }
    val untpdSplice = tpd.UntypedSplice(addRefs.transform(original)).withType(typed.tpe)
    seq(implicitBindings, untpdSplice)
  }

  /** `sym` has an inline method with a known body to inline (note: definitions coming
   *  from Scala2x class files might be `@forceInline`, but still lack that body.
   */
  def hasBodyToInline(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isInlineableMethod && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.unforcedAnnotation(defn.BodyAnnot).get.tree

  /** Try to inline a call to a `inline` method. Fail with error if the maximal
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
      if (ctx.reporter.hasErrors) tree else new Inliner(tree, body).inlined(pt)
    }
    else errorTree(
      tree,
      i"""|Maximal number of successive inlines (${ctx.settings.XmaxInlines.value}) exceeded,
          |Maybe this is caused by a recursive inline method?
          |You can use -Xmax:inlines to change the limit."""
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

  /** The qualifier part of a Select or Ident.
   *  For an Ident, this is the `This` of the current class. (TODO: use elsewhere as well?)
   */
  private def qualifier(tree: Tree)(implicit ctx: Context) = tree match {
    case Select(qual, _) => qual
    case _ => This(ctx.owner.enclosingClass.asClass)
  }
}

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call         the original call to an `inline` method
 *  @param  rhsToInline  the body of the inline method that replaces the call.
 */
class Inliner(call: tpd.Tree, rhsToInline: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private val (methPart, targs, argss) = decomposeCall(call)
  private val meth = methPart.symbol
  private val prefix = qualifier(methPart)

  // Make sure all type arguments to the call are fully determined
  for (targ <- targs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

  /** A map from parameter names of the inline method to references of the actual arguments.
   *  For a type argument this is the full argument type.
   *  For a value argument, it is a reference to either the argument value
   *  (if the argument is a pure expression of singleton type), or to `val` or `def` acting
   *  as a proxy (if the argument is something else).
   */
  private val paramBinding = new mutable.HashMap[Name, Type]

  /** A map from references to (type and value) parameters of the inline method
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
    val inlineFlag = if (paramtp.hasAnnotation(defn.InlineParamAnnot)) Inline else EmptyFlags
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

    val inlineTyper = if (meth.isTransparentMethod) new TransparentTyper else new InlineReTyper
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
            // it seems non-trivial to come up with rules that work in
          inlineCtx.enter(idef.symbol)
        }
        expansion
      case tpd.UntypedSplice(expansion) =>
        expansion
      case expansion =>
        expansion
    }

    trace(i"inlining $call", inlining, show = true) {

      // The final expansion runs a typing pass over the inlined tree. See InlineTyper for details.
      val expansion1 = inlineTyper.typed(expansion, pt)(inlineCtx)

      /** All bindings in `bindingsBuf` except bindings of inlineable closures */
      val bindings = bindingsBuf.toList.map(_.withPos(call.pos))

      if (ctx.settings.verbose.value) {
        inlining.println(i"original bindings = $bindings%\n%")
        inlining.println(i"original expansion = $expansion1")
      }

      val (finalBindings, finalExpansion) = dropUnusedDefs(bindings, expansion1)

      tpd.Inlined(call, finalBindings, finalExpansion)
    }
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
          case Some(vdef: ValDef) if vdef.symbol.is(Inline) =>
            Some(vdef.rhs.changeOwner(vdef.symbol, ctx.owner))
          case Some(ddef: DefDef) =>
            Some(ddef.rhs.changeOwner(ddef.symbol, ctx.owner))
          case _ => None
        }
      else None
  }

  /** A base trait of InlineTyper and InlineReTyper containing operations that
   *  work the same way for both. Beyond typing or retyping, an inline typer performs
   *  the following functions:
   *
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are by-name closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  trait InlineTyping extends Typer {

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

      betaReduce(super.typedApply(tree, pt))
    }
  }

  /** A full typer used for transparent methods */
  private class TransparentTyper extends Typer with InlineTyping {

    override def typedTypedSplice(tree: untpd.TypedSplice)(implicit ctx: Context): Tree =
      tree.splice match {
        case InlineableArg(rhs) => inlining.println(i"inline arg $tree -> $rhs"); rhs
        case _ => super.typedTypedSplice(tree)
      }

    /** Pre-type any nested calls to transparent methods. Otherwise the declared result type
     *  of these methods can influence constraints
     */
    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) = {
      def typeTransparent(tree: untpd.Tree): untpd.Tree =
        if (tree.symbol.isTransparentMethod) untpd.TypedSplice(typed(tree))
        else tree
      val tree1 = tree.args.mapConserve(typeTransparent)
      super.typedApply(untpd.cpy.Apply(tree)(tree.fun, tree1), pt)
    }
  }

  /** A re-typer used for inlined methods */
  private class InlineReTyper extends ReTyper with InlineTyping {

    override def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
      tpe match {
        case tpe: NamedType if !tpe.symbol.isAccessibleFrom(tpe.prefix, superAccess) =>
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
      tree.asInstanceOf[tpd.Tree] match {
        case InlineableArg(rhs) => inlining.println(i"inline arg $tree -> $rhs"); rhs
        case _ => super.typedIdent(tree, pt)
      }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.hasType, tree)
      val qual1 = typed(tree.qualifier, selectionProto(tree.name, pt, this))
      val res = untpd.cpy.Select(tree)(qual1, tree.name).withType(tree.typeOpt)
      ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.pos)
      res
    }

    override def newLikeThis: Typer = new InlineReTyper
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
