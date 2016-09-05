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
import SymDenotations.SymDenotation
import Annotations.Annotation
import transform.ExplicitOuter
import config.Printers.inlining
import ErrorReporting.errorTree
import util.{Property, SourceFile, NoSource}
import collection.mutable

object Inliner {
  import tpd._

  private class InlinedBody(tree: => Tree) {
    lazy val body = tree
  }

  private val InlinedBody = new Property.Key[InlinedBody] // to be used as attachment

  private val InlinedCalls = new Property.Key[List[Tree]] // to be used in context

  def attachBody(inlineAnnot: Annotation, tree: => Tree)(implicit ctx: Context): Unit =
    inlineAnnot.tree.putAttachment(InlinedBody, new InlinedBody(tree))

  def inlinedBody(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.getAnnotation(defn.InlineAnnot).get.tree
      .attachment(InlinedBody).body

  private object InlineTyper extends ReTyper {
    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      val acc = tree.symbol
      super.typedSelect(tree, pt) match {
        case res @ Select(qual, name) =>
          if (name.endsWith(nme.OUTER)) {
            val outerAcc = tree.symbol
            println(i"selecting $tree / ${acc} / ${qual.tpe.normalizedPrefix}")
            res.withType(qual.tpe.widen.normalizedPrefix)
          }
          else {
            ensureAccessible(res.tpe, qual.isInstanceOf[Super], tree.pos)
            res
          }
        case res => res
      }
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
}

class Inliner(call: tpd.Tree, rhs: tpd.Tree)(implicit ctx: Context) {
  import tpd._
  import Inliner._

  private def decomposeCall(tree: Tree): (Tree, List[Tree], List[List[Tree]]) = tree match {
    case Apply(fn, args) =>
      val (meth, targs, argss) = decomposeCall(fn)
      (meth, targs, argss :+ args)
    case TypeApply(fn, targs) =>
      val (meth, Nil, Nil) = decomposeCall(fn)
      (meth, targs, Nil)
    case _ =>
      (tree, Nil, Nil)
  }

  private val (methPart, targs, argss) = decomposeCall(call)
  private val meth = methPart.symbol

  private val prefix = methPart match {
    case Select(qual, _) => qual
    case _ => tpd.This(ctx.owner.enclosingClass.asClass)
  }

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
        paramBinding(name) = arg.tpe.stripTypeVar match {
          case argtpe: TypeRef => argtpe
          case argtpe =>
            val binding = newSym(name, EmptyFlags, TypeAlias(argtpe)).asType
            bindingsBuf += TypeDef(binding)
            binding.typeRef
        }
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
    case _: This | _: Ident => registerType(tree.tpe)
    case _ =>
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
        case t: SingletonType => paramProxy.getOrElse(t, t)
        case t => mapOver(t)
      }
    }

    def treeMap(tree: Tree) = tree match {
      case _: This =>
        thisProxy.get(tree.tpe) match {
          case Some(t) => ref(t)
          case None => tree
        }
      case _: Ident =>
        paramProxy.get(tree.tpe) match {
          case Some(t: TypeRef) => ref(t)
          case Some(t: SingletonType) => singleton(t)
          case None => tree
        }
      case _ => tree
    }

    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)
    val bindings = bindingsBuf.toList.map(_.withPos(call.pos))
    val expansion = inliner(rhs.withPos(call.pos))

    val expansion1 = InlineTyper.typed(expansion, pt)(inlineContext(call))
    val result = tpd.Inlined(call, bindings, expansion1)

    inlining.println(i"inlining $call\n --> \n$result")
    result
  }
}
