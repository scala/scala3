package dotty.tools.dotc

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd
import core._
import Types._, Symbols._, Annotations._, Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.OverridingPairs
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, MiniPhase, TransformerInfo, MiniPhaseTransform}
import dotty.tools.dotc.typer._
import ErrorReporting._
import Decorators._

import scala.annotation.tailrec


object Reim {
  implicit class TypeDecorator(val tpe: Type) extends AnyVal {
    def withAnnotation(cls: ClassSymbol)(implicit ctx: Context) =
      AnnotatedType(Annotation(cls, Nil), tpe.removeAnnotations)
    def removeAnnotations(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType if isAnnotation(tpe.annot.symbol) => tpe.tpe.removeAnnotations
      case tpe: AnnotatedType => tpe.derivedAnnotatedType(tpe.annot, tpe.tpe.removeAnnotations)
      case _ => tpe
    }
    def isAnnotated(implicit ctx: Context): Boolean = tpe.getAnnotation != NoSymbol
    def getAnnotation(implicit ctx: Context): Symbol = {
      checkAnnots(tpe)
      tpe match {
        case tpe: AnnotatedType =>
          if(isAnnotation(tpe.annot.symbol)) tpe.annot.symbol else tpe.tpe.getAnnotation
        case tpe: TermRef => tpe.underlying.getAnnotation
        case _ => NoSymbol
      }
    }
    def keepFirstAnnotation(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType if isAnnotation(tpe.annot.symbol) =>
        tpe.derivedAnnotatedType(tpe.annot, tpe.tpe.removeAnnotations)
      case tpe: AnnotatedType => tpe.derivedAnnotatedType(tpe.annot, tpe.tpe.keepFirstAnnotation)
      case _ => tpe
    }
  }

  implicit class TreeDecorator(val tree: tpd.Tree) extends AnyVal {
    def withAnnotation(cls: ClassSymbol)(implicit ctx: Context) = tree.withType(tree.tpe.withAnnotation(cls))
  }

  private def isAnnotation(symbol: Symbol)(implicit ctx: Context) =
    (symbol == defn.MutableAnnot) || (symbol == defn.ReadOnlyAnnot)

  private def checkAnnots(tpe: Type)(implicit ctx: Context): Type = {
    assert(!(tpe.hasAnnotation(defn.ReadOnlyAnnot) && tpe.hasAnnotation(defn.MutableAnnot)))
    tpe
  }
  
  class ReimTyper extends Typer {
    
    override def adapt(tree: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      val tree1 = super.adapt(tree, pt, original) match {

        // make annotation ascription override any existing annotation
        case tree: tpd.Annotated =>
          tree.withType(tree.tpe.keepFirstAnnotation)
        case tree: tpd.Typed if original.isInstanceOf[untpd.Annotated] =>
          val tpe = tree.tpe.keepFirstAnnotation
          tpd.cpy.Typed(tree)(tree.expr, tree.tpt.withType(tpe))

        // make literals and newly allocated objects mutable
        case tree: tpd.Literal =>
          tree.withAnnotation(defn.MutableAnnot)
        case tree: tpd.New => tree.tpe match {
          case tpe: TypeRef if isAnnotation(tpe.symbol) => tree // don't recursively annotate the New of an annotation
          case _ => tree.withAnnotation(defn.MutableAnnot)
        }

        // add @mutable as default type annotation
        case tree: tpd.TypeApply => {
          val annotatedArgs = tree.args.mapConserve{arg =>
            if(arg.tpe.isAnnotated) arg else arg.withAnnotation(defn.MutableAnnot)
          }
          tpd.cpy.TypeApply(tree)(tree.fun, annotatedArgs)
        }
        case tree: tpd.SelectFromTypeTree =>
          if(tree.tpe.isAnnotated) tree else tree.withAnnotation(defn.MutableAnnot)
        case tree: tpd.Typed =>
          if(tree.tpt.tpe.isAnnotated) tree
          else tpd.cpy.Typed(tree)(tree.expr, tree.tpt.withAnnotation(defn.MutableAnnot))
        case tree: tpd.ValDef =>
          if(tree.tpt.tpe.isAnnotated) tree
          else tpd.cpy.ValDef(tree)(tree.name, tree.tpt.withAnnotation(defn.MutableAnnot), tree.rhs)
        case tree: tpd.DefDef =>
          if(tree.tpt.tpe.isAnnotated) tree
          else tpd.cpy.DefDef(tree)(tree.name, tree.tparams, tree.vparamss, tree.tpt.withAnnotation(defn.MutableAnnot), tree.rhs)


        // adjust select for transitivity of @readonly
        case tree: tpd.Select =>
          if(tree.qualifier.tpe.getAnnotation == defn.ReadOnlyAnnot || tree.symbol.info.getAnnotation == defn.ReadOnlyAnnot)
            tree.withAnnotation(defn.ReadOnlyAnnot)
          else tree

        case tree => tree
      }
      val ptAnnot = pt.getAnnotation
      val treeAnnot = tree1.tpe.getAnnotation
      val dontCheck = pt == WildcardType || pt.isInstanceOf[ProtoType]
      if(!dontCheck && (ptAnnot != defn.ReadOnlyAnnot) && (treeAnnot == defn.ReadOnlyAnnot))
        err.typeMismatch(tree1, pt)
      tree1
    }
  }

  class ReimPhase extends MiniPhaseTransform {
    override def phaseName: String = "reim"

    override def transformAssign(tree: tpd.Assign)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      tree.lhs match {
        case lhs: tpd.Select if(lhs.qualifier.tpe.getAnnotation == defn.ReadOnlyAnnot) =>
          errorTree(tree, "assignment to field of @readonly reference")
        case lhs: tpd.Ident => lhs.tpe match {
          case TermRef(ThisType(thisTpe), _) if !isThisMutable(thisTpe.classSymbol) =>
            errorTree(tree, "assignment to field of @readonly reference")
          case _ => tree
        }
        case lhs: tpd.Select => tree
      }
    }

    def isThisMutable(thisSym: Symbol)(implicit ctx: Context): Boolean = {
      val owner = ctx.owner
      assert(owner ne null)
      assert(owner != NoSymbol)
//      if(owner eq null) return true
//      if(owner == NoSymbol) return true
      if (owner == thisSym || ((owner is Flags.Method) && (owner.owner == thisSym)))
        !owner.hasAnnotation(defn.ReadOnlyAnnot)
//      else if(ctx.outer eq null) true
      else isThisMutable(thisSym)(ctx.outer)
    }
  }

  class ReimTypeComparer(initctx: Context) extends TypeComparer(initctx) {
    override def isSubType(tp1: Type, tp2: Type): Boolean = super.isSubType(tp1, tp2) && {
      val annot1 = tp1.getAnnotation
      val annot2 = tp2.getAnnotation
      annot1 != defn.ReadOnlyAnnot || annot2 == defn.ReadOnlyAnnot
    }

    override def copyIn(ctx: Context): TypeComparer = new ReimTypeComparer(ctx)
  }

  class ReimRefChecks extends RefChecks {
    override def run(implicit ctx: Context): Unit = super.run(ctx.fresh.setTypeComparerFn(c => new ReimTypeComparer(c)))
  }

  class ReimRefChecks2 extends MiniPhase { thisTransformer =>
    val treeTransform = new TreeTransform {

      override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        @tailrec def receiverAndMethod(tree: tpd.Tree): (Type, Symbol) = tree match {
          case tree: tpd.Select => (tree.qualifier.tpe, tree.symbol)
          case tree: tpd.Ident => tree.tpe match {
            case tr: TermRef => (tr.prefix, tr.symbol)
          }
          case tree: tpd.TypeApply => receiverAndMethod(tree.fun) // polymorphic method
          case tree: tpd.Apply => receiverAndMethod(tree.fun) // multiple parameter lists
        }
        val (receiverType, methodSym) = receiverAndMethod(tree.fun)
        if((receiverType.getAnnotation == defn.ReadOnlyAnnot) && !methodSym.hasAnnotation(defn.ReadOnlyAnnot)) {
          errorTree(tree, "call of method taking @mutable this on @readonly receiver")
        } else  tree
      }

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        val cursor = new OverridingPairs.Cursor(ctx.owner)
        while (cursor.hasNext) {
          if(!cursor.overriding.hasAnnotation(defn.ReadOnlyAnnot) &&
          cursor.overridden.hasAnnotation(defn.ReadOnlyAnnot))
            ctx.error("method with @mutable this cannot override method with @readonly this", cursor.overriding.pos)
          cursor.next()
        }
        tree
      }

      def phase = thisTransformer
    }

    def phaseName: String = "reimrefchecks2"
  }
}

