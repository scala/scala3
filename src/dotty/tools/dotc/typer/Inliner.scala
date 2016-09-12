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
import Names.{Name, TermName}
import NameOps._
import SymDenotations.SymDenotation
import Annotations.Annotation
import transform.ExplicitOuter
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import util.{Property, SourceFile, NoSource}
import collection.mutable
import transform.TypeUtils._

/** Todo wrt inline SIP:
 *
 *  1. According to Inline SIP, by-name parameters are not hoisted out, but we currently
 *  do hoist them.
 *
 *  2. Inline call-by-name parameters are currently ignored. Not sure what the rules should be.
 */
object Inliner {
  import tpd._

  /** An attachment for inline methods, which contains
   *
   *   - the body to inline, as a typed tree
   *   - the definitions of all needed accessors to non-public members from inlined code
   *
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param inlineCtxFn A function that maps the current context to the context in
   *                     which to compute the tree. The resulting context needs
   *                     to have the inlined method as owner.
   *
   *                     The reason to use a function rather than a fixed context here
   *                     is to avoid space leaks. InlineInfos can survive multiple runs
   *                     because they might be created as part of an unpickled method
   *                     and consumed only in a future run (or never).
   */
  private final class InlineInfo(treeExpr: Context => Tree, var inlineCtxFn: Context => Context) {
    private val myAccessors = new mutable.ListBuffer[MemberDef]
    private var myBody: Tree = _
    private var evaluated = false

    /** A tree map which inserts accessors for all non-public term members accessed
     *  from inlined code. Non-public type members are currently left as they are.
     *  This means that references to a provate type will lead to typing failures
     *  on the code when it is inlined. Less than ideal, but hard to do better (see below).
     */
    private def prepareForInline(inlineCtx: Context) = new TreeMap {
      val inlineMethod = inlineCtx.owner

      /** A definition needs an accessor if it is private, protected, or qualified private */
      def needsAccessor(sym: Symbol)(implicit ctx: Context) =
        sym.is(AccessFlags) || sym.privateWithin.exists

      /** The name of the next accessor to be generated */
      def accessorName(implicit ctx: Context) =
        ctx.freshNames.newName(inlineMethod.name.asTermName.inlineAccessorName.toString)

      /** A fresh accessor symbol.
       *
       *  @param tree          The tree representing the original access to the non-public member
       *  @param accessorInfo  The type of the accessor
       */
      def accessorSymbol(tree: Tree, accessorInfo: Type)(implicit ctx: Context): Symbol =
        ctx.newSymbol(
          owner = inlineMethod.owner,
          name = if (tree.isTerm) accessorName.toTermName else accessorName.toTypeName,
          flags = if (tree.isTerm) Synthetic | Method else Synthetic,
          info = accessorInfo,
          coord = tree.pos).entered

      /** Add an accessor to a non-public method and replace the original access with a
       *  call to the accessor.
       *
       *  @param tree         The original access to the non-public symbol
       *  @param refPart      The part that refers to the method or field of the original access
       *  @param targs        All type arguments passed in the access, if any
       *  @param argss        All value arguments passed in the access, if any
       *  @param accessedType The type of the accessed method or field, as seen from the access site.
       *  @param rhs          A function that builds the right-hand side of the accessor,
       *                      given a reference to the accessed symbol and any type and
       *                      value arguments the need to be integrated.
       *  @return The call to the accessor method that replaces the original access.
       */
      def addAccessor(tree: Tree, refPart: Tree, targs: List[Tree], argss: List[List[Tree]],
                      accessedType: Type, rhs: (Tree, List[Type], List[List[Tree]]) => Tree)(implicit ctx: Context): Tree = {
        val (accessorDef, accessorRef) =
          if (refPart.symbol.isStatic) {
            // Easy case: Reference to a static symbol
            val accessorType = accessedType.ensureMethodic
            val accessor = accessorSymbol(tree, accessorType).asTerm
            val accessorDef = polyDefDef(accessor, tps => argss =>
              rhs(refPart, tps, argss))
            val accessorRef = ref(accessor).appliedToTypeTrees(targs).appliedToArgss(argss)
            (accessorDef, accessorRef)
          } else {
            // Hard case: Reference needs to go via a dyanmic prefix
            val qual = qualifier(refPart)
            inlining.println(i"adding inline accessor for $tree -> (${qual.tpe}, $refPart: ${refPart.getClass}, [$targs%, %], ($argss%, %))")

            // Need to dealias in order to catch all possible references to abstracted over types in
            // substitutions
            val dealiasMap = new TypeMap {
              def apply(t: Type) = mapOver(t.dealias)
            }

            val qualType = dealiasMap(qual.tpe.widen)

            // Add qualifier type as leading method argument to argument `tp`
            def addQualType(tp: Type): Type = tp match {
              case tp: PolyType => tp.derivedPolyType(tp.paramNames, tp.paramBounds, addQualType(tp.resultType))
              case tp: ExprType => addQualType(tp.resultType)
              case tp => MethodType(qualType :: Nil, tp)
            }

            // The types that are local to the inlined method, and that therefore have
            // to be abstracted out in the accessor, which is external to the inlined method
            val localRefs = qualType.namedPartsWith(_.symbol.isContainedIn(inlineMethod)).toList

            // Abstract accessed type over local refs
            def abstractQualType(mtpe: Type): Type =
              if (localRefs.isEmpty) mtpe
              else PolyType.fromSymbols(localRefs.map(_.symbol), mtpe).asInstanceOf[PolyType].flatten

            val accessorType = abstractQualType(addQualType(dealiasMap(accessedType)))
            val accessor = accessorSymbol(tree, accessorType).asTerm

            val accessorDef = polyDefDef(accessor, tps => argss =>
              rhs(argss.head.head.select(refPart.symbol), tps.drop(localRefs.length), argss.tail))

            val accessorRef = ref(accessor)
              .appliedToTypeTrees(localRefs.map(TypeTree(_)) ++ targs)
              .appliedToArgss((qual :: Nil) :: argss)
            (accessorDef, accessorRef)
          }
        myAccessors += accessorDef
        inlining.println(i"added inline accessor: $accessorDef")
        accessorRef
      }

      override def transform(tree: Tree)(implicit ctx: Context): Tree = super.transform {
        tree match {
          case _: Apply | _: TypeApply | _: RefTree if needsAccessor(tree.symbol) =>
            if (tree.isTerm) {
              val (methPart, targs, argss) = decomposeCall(tree)
              addAccessor(tree, methPart, targs, argss,
                  accessedType = methPart.tpe.widen,
                  rhs = (qual, tps, argss) => qual.appliedToTypes(tps).appliedToArgss(argss))
           } else {
              // TODO: Handle references to non-public types.
              // This is quite tricky, as such types can appear anywhere, including as parts
              // of types of other things. For the moment we do nothing and complain
              // at the implicit expansion site if there's a reference to an inaccessible type.
              // Draft code (incomplete):
              //
              //  val accessor = accessorSymbol(tree, TypeAlias(tree.tpe)).asType
              //  myAccessors += TypeDef(accessor)
              //  ref(accessor)
              //
              tree
            }
          case Assign(lhs: RefTree, rhs) if needsAccessor(lhs.symbol) =>
            addAccessor(tree, lhs, Nil, (rhs :: Nil) :: Nil,
                accessedType = MethodType(rhs.tpe.widen :: Nil, defn.UnitType),
                rhs = (lhs, tps, argss) => lhs.becomes(argss.head.head))
          case _ => tree
        }
      }
    }

    /** Is the inline info evaluated? */
    def isEvaluated = evaluated

    private def ensureEvaluated()(implicit ctx: Context) =
      if (!evaluated) {
        evaluated = true // important to set early to prevent overwrites by attachInlineInfo in typedDefDef
        val inlineCtx = inlineCtxFn(ctx)
        inlineCtxFn = null // null out to avoid space leaks
        try {
          myBody = treeExpr(inlineCtx)
          myBody = prepareForInline(inlineCtx).transform(myBody)(inlineCtx)
          inlining.println(i"inlinable body of ${inlineCtx.owner} = $myBody")
        }
        catch {
          case ex: AssertionError =>
            println(i"failure while expanding ${inlineCtx.owner}")
            throw ex
        }
      }
      else assert(myBody != null)

    /** The body to inline */
    def body(implicit ctx: Context): Tree = {
      ensureEvaluated()
      myBody
    }

    /** The accessor defs to non-public members which need to be defined
     *  together with the inline method
     */
    def removeAccessors(implicit ctx: Context): List[MemberDef] = {
      ensureEvaluated()
      val res = myAccessors.toList
      myAccessors.clear()
      res
    }
  }

  /** A key to be used in an attachment for `@inline` annotations */
  private val InlineInfo = new Property.Key[InlineInfo]

  /** A key to be used in a context property that tracks enclosing inlined calls */
  private val InlinedCalls = new Property.Key[List[Tree]] // to be used in context

  /** Register inline info for given inline method `sym`.
   *
   *  @param sym         The symbol denotatioon of the inline method for which info is registered
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param inlineCtxFn A function that maps the current context to the context in
   *                     which to compute the tree. The resulting context needs
   *                     to have the inlined method as owner.
   */
  def registerInlineInfo(
      sym: SymDenotation, treeExpr: Context => Tree, inlineCtxFn: Context => Context)(implicit ctx: Context): Unit = {
    val inlineAnnot = sym.unforcedAnnotation(defn.InlineAnnot).get
    inlineAnnot.tree.getAttachment(InlineInfo) match {
      case Some(inlineInfo) if inlineInfo.isEvaluated => // keep existing attachment
      case _ =>
        if (!ctx.isAfterTyper)
          inlineAnnot.tree.putAttachment(InlineInfo, new InlineInfo(treeExpr, inlineCtxFn))
    }
  }

  /** Optionally, the inline info attached to the `@inline` annotation of `sym`. */
  private def inlineInfo(sym: SymDenotation)(implicit ctx: Context): Option[InlineInfo] =
    sym.getAnnotation(defn.InlineAnnot).get.tree.getAttachment(InlineInfo)

  /** Definition is an inline method with a known body to inline (note: definitions coming
   *  from Scala2x class files might be `@inline`, but still lack that body.
   */
  def hasBodyToInline(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isInlineMethod && inlineInfo(sym).isDefined

  /** The body to inline for method `sym`.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(implicit ctx: Context): Tree =
    inlineInfo(sym).get.body

 /** The accessors to non-public members needed by the inlinable body of `sym`.
   * @pre  hasBodyToInline(sym)
   */

  def removeInlineAccessors(sym: SymDenotation)(implicit ctx: Context): List[MemberDef] =
    inlineInfo(sym).get.removeAccessors

  /** Try to inline a call to a `@inline` method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @param pt     The expected type of the call.
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
    if (enclosingInlineds.length < ctx.settings.xmaxInlines.value)
      new Inliner(tree, bodyToInline(tree.symbol)).inlined(pt)
    else errorTree(tree,
      i"""Maximal number of successive inlines (${ctx.settings.xmaxInlines.value}) exceeded,
                   | Maybe this is caused by a recursive inline method?
                   | You can use -Xmax:inlines to change the limit.""")

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
    val reposition = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree =
        tree.withPos(inlined.call.pos)
    }
    tpd.seq(inlined.bindings, reposition.transform(inlined.expansion))
  }

  /** A context derived form `ctx` that records `call` as innermost enclosing
   *  call for which the inlined version is currently processed.
   */
  def inlineContext(call: Tree)(implicit ctx: Context): Context =
    ctx.fresh.setProperty(InlinedCalls, call :: enclosingInlineds)

  /** All enclosing calls that are currently inlined, from innermost to outermost */
  def enclosingInlineds(implicit ctx: Context): List[Tree] =
    ctx.property(InlinedCalls).getOrElse(Nil)

  /** The source file where the symbol of the `@inline` method referred to by `call`
   *  is defined
   */
  def sourceFile(call: Tree)(implicit ctx: Context) = {
    val file = call.symbol.sourceFile
    if (file != null && file.exists) new SourceFile(file) else NoSource
  }

  /** The qualifier part of a Select, Ident, or SelectFromTypeTree tree.
   *  For an Ident, this is the `This` of the current class. (TODO: use elsewhere as well?)
   */
  private def qualifier(tree: Tree)(implicit ctx: Context) = tree match {
    case Select(qual, _) => qual
    case SelectFromTypeTree(qual, _) => qual
    case _ => This(ctx.owner.enclosingClass.asClass)
  }
}

/** Produces an inlined version of `call` via its `inlined` method.
 *
 *  @param  call  The original call to a `@inline` method
 *  @param  rhs   The body of the inline method that replaces the call.
 */
class Inliner(call: tpd.Tree, rhs: tpd.Tree)(implicit ctx: Context) {
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

  /** A map from (direct and outer) this references in `rhs` to references of their proxies */
  private val thisProxy = new mutable.HashMap[Type, TermRef]

  /** A buffer for bindings that define proxies for actual arguments */
  val bindingsBuf = new mutable.ListBuffer[ValOrDefDef]

  computeParamBindings(meth.info, targs, argss)

  private def newSym(name: Name, flags: FlagSet, info: Type): Symbol =
    ctx.newSymbol(ctx.owner, name, flags, info, coord = call.pos)

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
      (tp.paramNames, tp.paramTypes, argss.head).zipped.foreach { (name, paramtp, arg) =>
        def isByName = paramtp.dealias.isInstanceOf[ExprType]
        paramBinding(name) = arg.tpe.stripTypeVar match {
          case argtpe: SingletonType if isByName || isIdempotentExpr(arg) => argtpe
          case argtpe =>
            val (bindingFlags, bindingType) =
              if (isByName) (Method, ExprType(argtpe.widen)) else (EmptyFlags, argtpe.widen)
            val boundSym = newSym(name, bindingFlags, bindingType).asTerm
            val binding =
              if (isByName) DefDef(boundSym, arg.changeOwner(ctx.owner, boundSym))
              else ValDef(boundSym, arg)
            bindingsBuf += binding
            boundSym.termRef
        }
      }
      computeParamBindings(tp.resultType, targs, argss.tail)
    case _ =>
      assert(targs.isEmpty)
      assert(argss.isEmpty)
  }

  /** Populate `thisProxy` and `paramProxy` as follows:
   *
   *  1a. If given type refers to a static this, thisProxy binds it to corresponding global reference,
   *  1b. If given type refers to an instance this, create a proxy symbol and bind the thistype to
   *      refer to the proxy. The proxy is not yet entered in `bindingsBuf` that will come later.
   *  2.  If given type refers to a parameter, make `paramProxy` refer to the entry stored
   *      in `paramNames` under the parameter's name. This roundabout way to bind parameter
   *      references to proxies is done because  we not known a priori what the parameter
   *      references of a method are (we only know the method's type, but that contains PolyParams
   *      and MethodParams, not TypeRefs or TermRefs.
   */
  private def registerType(tpe: Type): Unit = tpe match {
    case tpe: ThisType
    if !ctx.owner.isContainedIn(tpe.cls) && !tpe.cls.is(Package) &&
       !thisProxy.contains(tpe) =>
      if (tpe.cls.isStaticOwner)
        thisProxy(tpe) = tpe.cls.sourceModule.termRef
      else {
        val proxyName = s"${tpe.cls.name}_this".toTermName
        val proxyType = tpe.asSeenFrom(prefix.tpe, meth.owner)
        thisProxy(tpe) = newSym(proxyName, EmptyFlags, proxyType).termRef
        registerType(meth.owner.thisType) // make sure we have a base from which to outer-select
      }
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
    rhs.foreachSubTree(registerLeaf)

    // The class that the this-proxy `selfSym` represents
    def classOf(selfSym: Symbol) = selfSym.info.widen.classSymbol

    // The name of the outer selector that computes the rhs of `selfSym`
    def outerSelector(selfSym: Symbol): TermName = classOf(selfSym).name.toTermName ++ nme.OUTER_SELECT

    // The total nesting depth of the class represented by `selfSym`.
    def outerLevel(selfSym: Symbol): Int = classOf(selfSym).ownersIterator.length

    // All needed this-proxies, sorted by nesting depth of the classes they represent (innermost first)
    val accessedSelfSyms = thisProxy.values.toList.map(_.symbol).sortBy(-outerLevel(_))

    // Compute val-definitions for all this-proxies and append them to `bindingsBuf`
    var lastSelf: Symbol = NoSymbol
    for (selfSym <- accessedSelfSyms) {
      val rhs =
        if (!lastSelf.exists)
          prefix
        else
          untpd.Select(ref(lastSelf), outerSelector(selfSym)).withType(selfSym.info)
      bindingsBuf += ValDef(selfSym.asTerm, rhs)
      lastSelf = selfSym
    }

    // The type map to apply to the inlined tree. This maps references to this-types
    // and parameters to type references of their arguments or proxies.
    val typeMap = new TypeMap {
      def apply(t: Type) = t match {
        case t: ThisType => thisProxy.getOrElse(t, t)
        case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
        case t: SingletonType => paramProxy.getOrElse(t, mapOver(t))
        case t => mapOver(t)
      }
    }

    // The tree map to apply to the inlined tree. This maps references to this-types
    // and parameters to references of their arguments or their proxies.
    def treeMap(tree: Tree) = {
      tree match {
      case _: This =>
        thisProxy.get(tree.tpe) match {
          case Some(t) => ref(t).withPos(tree.pos)
          case None => tree
        }
      case _: Ident =>
        paramProxy.get(tree.tpe) match {
          case Some(t: SingletonType) if tree.isTerm => singleton(t).withPos(tree.pos)
          case Some(t) if tree.isType => TypeTree(t).withPos(tree.pos)
          case None => tree
        }
      case _ => tree
    }}

    // The complete translation maps referenves to this and parameters to
    // corresponding arguments or proxies on the type and term level. It also changes
    // the owner from the inlined method to the current owner.
    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)

    val expansion = inliner(rhs.withPos(call.pos))

    // The final expansion runs a typing pass over the inlined tree. See InlineTyper for details.
    val expansion1 = InlineTyper.typed(expansion, pt)(inlineContext(call))

    /** Does given definition bind a closure that will be inlined? */
    def bindsInlineableClosure(defn: ValOrDefDef) = Ident(defn.symbol.termRef) match {
      case InlineableClosure(_) => true
      case _ => false
    }

    /** All bindings in `bindingsBuf` except bindings of inlineable closures */
    val bindings = bindingsBuf.toList.filterNot(bindsInlineableClosure).map(_.withPos(call.pos))

    val result = tpd.Inlined(call, bindings, expansion1)
    inlining.println(i"inlined $call\n --> \n$result")
    result
  }

  /** An extractor for references to closure arguments that refer to `@inline` methods */
  private object InlineableClosure {
    lazy val paramProxies = paramProxy.values.toSet
    def unapply(tree: Ident)(implicit ctx: Context): Option[Tree] =
      if (paramProxies.contains(tree.tpe)) {
        bindingsBuf.find(_.name == tree.name).map(_.rhs) match {
          case Some(Closure(_, meth, _)) if meth.symbol.isInlineMethod => Some(meth)
          case Some(Block(_, Closure(_, meth, _))) if meth.symbol.isInlineMethod => Some(meth)
          case _ => None
        }
      } else None
  }

  /** A typer for inlined code. Its purpose is:
   *  1. Implement constant folding over inlined code
   *  2. Selectively expand ifs with constant conditions
   *  3. Inline arguments that are inlineable closures
   *  4. Make sure inlined code is type-correct.
   *  5. Make sure that the tree's typing is idempotent (so that future -Ycheck passes succeed)
   */
  private object InlineTyper extends ReTyper {

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      val res = super.typedSelect(tree, pt)
      ensureAccessible(res.tpe, tree.qualifier.isInstanceOf[untpd.Super], tree.pos)
      res
    }

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) = {
      val cond1 = typed(tree.cond, defn.BooleanType)
      cond1.tpe.widenTermRefExpr match {
        case ConstantType(Constant(condVal: Boolean)) =>
          val selected = typed(if (condVal) tree.thenp else tree.elsep, pt)
          if (isIdempotentExpr(cond1)) selected
          else Block(cond1 :: Nil, selected)
        case _ =>
          val if1 = untpd.cpy.If(tree)(cond = untpd.TypedSplice(cond1))
          super.typedIf(if1, pt)
      }
    }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context) = tree.asInstanceOf[tpd.Tree] match {
      case Apply(Select(InlineableClosure(fn), nme.apply), args) =>
        typed(fn.appliedToArgs(args), pt)
      case _ =>
        super.typedApply(tree, pt)
    }
  }
}
