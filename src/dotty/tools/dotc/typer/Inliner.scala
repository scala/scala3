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
import StdNames.nme
import Contexts.Context
import Names.Name
import SymDenotations.SymDenotation
import Annotations.Annotation
import transform.ExplicitOuter
import config.Printers.inlining
import ErrorReporting.errorTree
import util.Property
import collection.mutable

object Inliner {
  import tpd._

  private class InlinedBody(tree: => Tree) {
    lazy val body = tree
  }

  private val InlinedBody = new Property.Key[InlinedBody] // to be used as attachment

  def attachBody(inlineAnnot: Annotation, tree: => Tree)(implicit ctx: Context): Unit =
    inlineAnnot.tree.putAttachment(InlinedBody, new InlinedBody(tree))

  def inlinedBody(sym: SymDenotation)(implicit ctx: Context): Tree =
    sym.getAnnotation(defn.InlineAnnot).get.tree
      .attachment(InlinedBody).body

  private class Typer extends ReTyper {
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
  }

  def inlineCall(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {
    if (ctx.inlineCount < ctx.settings.xmaxInlines.value) {
      ctx.inlineCount += 1
      val rhs = inlinedBody(tree.symbol)
      val inlined = new Inliner(tree, rhs).inlined
      try new Typer().typedUnadapted(inlined, pt)
      finally ctx.inlineCount -= 1
    } else errorTree(tree,
      i"""Maximal number of successive inlines (${ctx.settings.xmaxInlines.value}) exceeded,
                   | Maybe this is caused by a recursive inline method?
                   | You can use -Xmax:inlines to change the limit.""")
  }
}

class Inliner(call: tpd.Tree, rhs: tpd.Tree)(implicit ctx: Context) {
  import tpd._

  private val meth = call.symbol

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

  private lazy val prefix = methPart match {
    case Select(qual, _) => qual
    case _ => tpd.This(ctx.owner.enclosingClass.asClass)
  }

  private val replacement = new mutable.HashMap[Type, NamedType]

  private val paramBindings = paramBindingsOf(meth.info, targs, argss)

  private def paramBindingsOf(tp: Type, targs: List[Tree], argss: List[List[Tree]]): List[MemberDef] = tp match {
    case tp: PolyType =>
      val bindings =
        (tp.paramNames, targs).zipped.map { (name, arg) =>
          val tparam = newSym(name, EmptyFlags, TypeAlias(arg.tpe.stripTypeVar)).asType
          TypeDef(tparam)
        }
      bindings ::: paramBindingsOf(tp.resultType, Nil, argss)
    case tp: MethodType =>
      val bindings =
        (tp.paramNames, tp.paramTypes, argss.head).zipped.map { (name, paramtp, arg) =>
          def isByName = paramtp.dealias.isInstanceOf[ExprType]
          val (paramFlags, paramType) =
            if (isByName) (Method, ExprType(arg.tpe)) else (EmptyFlags, arg.tpe)
          val vparam = newSym(name, paramFlags, paramType).asTerm
          if (isByName) DefDef(vparam, arg) else ValDef(vparam, arg)
        }
      bindings ::: paramBindingsOf(tp.resultType, targs, argss.tail)
    case _ =>
      assert(targs.isEmpty)
      assert(argss.isEmpty)
      Nil
  }

  private def newSym(name: Name, flags: FlagSet, info: Type): Symbol =
    ctx.newSymbol(ctx.owner, name, flags, info, coord = call.pos)

  private def registerType(tpe: Type): Unit =
    if (!replacement.contains(tpe)) tpe match {
      case tpe: ThisType =>
        if (!ctx.owner.isContainedIn(tpe.cls) && !tpe.cls.is(Package))
          if (tpe.cls.isStaticOwner)
            replacement(tpe) = tpe.cls.sourceModule.termRef
          else {
            def outerDistance(cls: Symbol): Int = {
              assert(cls.exists, i"not encl: ${meth.owner.enclosingClass} ${tpe.cls}")
              if (tpe.cls eq cls) 0
              else outerDistance(cls.owner.enclosingClass) + 1
            }
            val n = outerDistance(meth.owner)
            replacement(tpe) = newSym(nme.SELF ++ n.toString, EmptyFlags, tpe.widen).termRef
          }
      case tpe: NamedType if tpe.symbol.is(Param) && tpe.symbol.owner == meth =>
        val Some(binding) = paramBindings.find(_.name == tpe.name)
        replacement(tpe) =
          if (tpe.name.isTypeName) binding.symbol.typeRef else binding.symbol.termRef
      case _ =>
    }

  private def registerLeaf(tree: Tree): Unit = tree match {
    case _: This | _: Ident => registerType(tree.tpe)
    case _ =>
  }

  private def outerLevel(sym: Symbol) = sym.name.drop(nme.SELF.length).toString.toInt

  val inlined = {
    rhs.foreachSubTree(registerLeaf)

    val accessedSelfSyms =
      (for ((tp: ThisType, ref) <- replacement) yield ref.symbol.asTerm).toSeq.sortBy(outerLevel)

    val outerBindings = new mutable.ListBuffer[MemberDef]
    for (selfSym <- accessedSelfSyms) {
      val rhs =
        if (outerBindings.isEmpty) prefix
        else {
          val lastSelf = outerBindings.last.symbol
          val outerDelta = outerLevel(selfSym) - outerLevel(lastSelf)
          def outerSelect(ref: Tree, dummy: Int): Tree = ???
            //ref.select(ExplicitOuter.outerAccessorTBD(ref.tpe.widen.classSymbol.asClass))
          (ref(lastSelf) /: (0 until outerDelta))(outerSelect)
        }
      outerBindings += ValDef(selfSym, rhs.ensureConforms(selfSym.info))
    }
    outerBindings ++= paramBindings

    val typeMap = new TypeMap {
      def apply(t: Type) = t match {
        case _: SingletonType => replacement.getOrElse(t, t)
        case _ => mapOver(t)
      }
    }

    def treeMap(tree: Tree) = tree match {
      case _: This | _: Ident =>
        replacement.get(tree.tpe) match {
          case Some(t) => ref(t)
          case None => tree
        }
      case _ => tree
    }

    val inliner = new TreeTypeMap(typeMap, treeMap, meth :: Nil, ctx.owner :: Nil)

    val result = inliner(Block(outerBindings.toList, rhs)).withPos(call.pos)

    inlining.println(i"inlining $call\n --> \n$result")
    result
  }
}
