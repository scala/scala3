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
import Names.Name
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

object Inliner {
  import tpd._

  /** An attachment for inline methods, which contains
   *
   *   - the inlined body, as a typed tree
   *   -
   *
   */
  private final class InlinedBody(treeExpr: Context => Tree, var inlineCtx: Context) {
    private val inlineMethod = inlineCtx.owner
    private val myAccessors = new mutable.ListBuffer[MemberDef]
    private var myBody: Tree = _
    private var evaluated = false

    private def prepareForInline = new TreeMap {

      def needsAccessor(sym: Symbol)(implicit ctx: Context) =
        sym.is(AccessFlags) || sym.privateWithin.exists

      def accessorName(implicit ctx: Context) =
        ctx.freshNames.newName(inlineMethod.name.asTermName.inlineAccessorName.toString)

      def accessorSymbol(tree: Tree, accessorInfo: Type)(implicit ctx: Context): Symbol =
        ctx.newSymbol(
          owner = inlineMethod.owner,
          name = if (tree.isTerm) accessorName.toTermName else accessorName.toTypeName,
          flags = if (tree.isTerm) Synthetic | Method else Synthetic,
          info = accessorInfo,
          coord = tree.pos).entered

      def addAccessor(tree: Tree, methPart: Tree, targs: List[Tree], argss: List[List[Tree]],
                      accessedType: Type, rhs: (Tree, List[Type], List[List[Tree]]) => Tree)(implicit ctx: Context): Tree = {
        val (accessorDef, accessorRef) =
          if (methPart.symbol.isStatic) {
            // Easy case: Reference to a static symbol
            val accessorType = accessedType.ensureMethodic
            val accessor = accessorSymbol(tree, accessorType).asTerm
            val accessorDef = polyDefDef(accessor, tps => argss =>
              rhs(methPart, tps, argss))
            val accessorRef = ref(accessor).appliedToTypeTrees(targs).appliedToArgss(argss)
            (accessorDef, accessorRef)
          } else {
            // Hard case: Reference needs to go via a dyanmic prefix
            val qual = qualifier(methPart)
            inlining.println(i"adding inline accessor for $tree -> (${qual.tpe}, $methPart: ${methPart.getClass}, [$targs%, %], ($argss%, %))")
            val dealiasMap = new TypeMap {
              def apply(t: Type) = mapOver(t.dealias)
            }
            val qualType = dealiasMap(qual.tpe.widen)
            def addQualType(tp: Type): Type = tp match {
              case tp: PolyType => tp.derivedPolyType(tp.paramNames, tp.paramBounds, addQualType(tp.resultType))
              case tp: ExprType => addQualType(tp.resultType)
              case tp => MethodType(qualType :: Nil, tp)
            }
            val localRefs = qualType.namedPartsWith(_.symbol.isContainedIn(inlineMethod)).toList
            def abstractQualType(mtpe: Type): Type =
              if (localRefs.isEmpty) mtpe
              else PolyType.fromSymbols(localRefs.map(_.symbol), mtpe).asInstanceOf[PolyType].flatten
            val accessorType = abstractQualType(addQualType(dealiasMap(accessedType)))
            val accessor = accessorSymbol(tree, accessorType).asTerm
            println(i"accessor: $accessorType")
            val accessorDef = polyDefDef(accessor, tps => argss =>
              rhs(argss.head.head.select(methPart.symbol), tps.drop(localRefs.length), argss.tail))
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

    def isEvaluated = evaluated
    private def ensureEvaluated()(implicit ctx: Context) =
      if (!evaluated) {
        evaluated = true
        myBody = treeExpr(inlineCtx)
        myBody = prepareForInline.transform(myBody)(inlineCtx)
        inlining.println(i"inlinable body of ${inlineCtx.owner} = $myBody")
        inlineCtx = null
      }

    def body(implicit ctx: Context): Tree = {
      ensureEvaluated()
      myBody
    }

    def accessors(implicit ctx: Context): List[MemberDef] = {
      ensureEvaluated()
      myAccessors.toList
    }
  }

  private val InlinedBody = new Property.Key[InlinedBody] // to be used as attachment

  private val InlinedCalls = new Property.Key[List[Tree]] // to be used in context

  def attachBody(inlineAnnot: Annotation, treeExpr: Context => Tree)(implicit ctx: Context): Unit =
    inlineAnnot.tree.getAttachment(InlinedBody) match {
      case Some(inlinedBody) if inlinedBody.isEvaluated => // keep existing attachment
      case _ =>
        if (!ctx.isAfterTyper)
          inlineAnnot.tree.putAttachment(InlinedBody, new InlinedBody(treeExpr, ctx))
    }

  private def inlinedBodyAttachment(sym: SymDenotation)(implicit ctx: Context): Option[InlinedBody] =
    sym.getAnnotation(defn.InlineAnnot).get.tree.getAttachment(InlinedBody)

  def hasInlinedBody(sym: SymDenotation)(implicit ctx: Context): Boolean =
    sym.isInlineMethod && inlinedBodyAttachment(sym).isDefined

  def inlinedBody(sym: SymDenotation)(implicit ctx: Context): Tree =
    inlinedBodyAttachment(sym).get.body

  def inlineAccessors(sym: SymDenotation)(implicit ctx: Context): List[MemberDef] =
    inlinedBodyAttachment(sym).get.accessors

  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
    if (enclosingInlineds.length < ctx.settings.xmaxInlines.value)
      new Inliner(tree, inlinedBody(tree.symbol)).inlined(pt)
    else errorTree(tree,
      i"""Maximal number of successive inlines (${ctx.settings.xmaxInlines.value}) exceeded,
                   | Maybe this is caused by a recursive inline method?
                   | You can use -Xmax:inlines to change the limit.""")

  def dropInlined(inlined: tpd.Inlined)(implicit ctx: Context): Tree = {
    val reposition = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree =
        tree.withPos(inlined.call.pos)
    }
    tpd.seq(inlined.bindings, reposition.transform(inlined.expansion))
  }

  def inlineContext(call: Tree)(implicit ctx: Context): Context =
    ctx.fresh.setProperty(InlinedCalls, call :: enclosingInlineds)

  def enclosingInlineds(implicit ctx: Context): List[Tree] =
    ctx.property(InlinedCalls).getOrElse(Nil)

  def sourceFile(call: Tree)(implicit ctx: Context) = {
    val file = call.symbol.sourceFile
    if (file != null && file.exists) new SourceFile(file) else NoSource
  }

  private def qualifier(tree: Tree)(implicit ctx: Context) = tree match {
    case Select(qual, _) => qual
    case SelectFromTypeTree(qual, _) => qual
    case _ => This(ctx.owner.enclosingClass.asClass)
  }
}

class Inliner(call: tpd.Tree, rhs: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private val (methPart, targs, argss) = decomposeCall(call)
  private val meth = methPart.symbol
  private val prefix = qualifier(methPart)

  for (targ <- targs) fullyDefinedType(targ.tpe, "inlined type argument", targ.pos)

  private val thisProxy = new mutable.HashMap[Type, TermRef]
  private val paramProxy = new mutable.HashMap[Type, Type]
  private val paramBinding = new mutable.HashMap[Name, Type]
  val bindingsBuf = new mutable.ListBuffer[MemberDef]

  computeParamBindings(meth.info, targs, argss)

  private def newSym(name: Name, flags: FlagSet, info: Type): Symbol =
    ctx.newSymbol(ctx.owner, name, flags, info, coord = call.pos)

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

  private def registerLeaf(tree: Tree): Unit = tree match {
    case _: This | _: Ident | _: TypeTree =>
      tree.tpe.foreachPart(registerType, stopAtStatic = true)
    case _ =>
  }

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
  }

  def inlined(pt: Type) = {
    if (!isIdempotentExpr(prefix)) registerType(meth.owner.thisType) // make sure prefix is computed
    rhs.foreachSubTree(registerLeaf)

    def classOf(sym: Symbol) = sym.info.widen.classSymbol
    def outerSelector(sym: Symbol) = classOf(sym).name.toTermName ++ nme.OUTER_SELECT
    def outerLevel(sym: Symbol) = classOf(sym).ownersIterator.length
    val accessedSelfSyms = thisProxy.values.toList.map(_.symbol).sortBy(-outerLevel(_))

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

    val typeMap = new TypeMap {
      def apply(t: Type) = t match {
        case t: ThisType => thisProxy.getOrElse(t, t)
        case t: TypeRef => paramProxy.getOrElse(t, mapOver(t))
        case t: SingletonType => paramProxy.getOrElse(t, mapOver(t))
        case t => mapOver(t)
      }
    }

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

    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)
    val bindings = bindingsBuf.toList.map(_.withPos(call.pos))
    val expansion = inliner(rhs.withPos(call.pos))

    val expansion1 = InlineTyper.typed(expansion, pt)(inlineContext(call))
    val result = tpd.Inlined(call, bindings, expansion1)

    inlining.println(i"inlined $call\n --> \n$result")
    result
  }
}
