package dotty.tools.dotc

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd
import core._
import Types._, Symbols._, Annotations._, Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import dotty.tools.dotc.typer.{FrontEnd, Mode, ErrorReporting, Typer}
import ErrorReporting._


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
      if(owner eq null) return true
      if(owner == NoSymbol) return true
      if (owner == thisSym || ((owner is Flags.Method) && (owner.owner == thisSym)))
        !owner.hasAnnotation(defn.ReadOnlyAnnot)
      else if(ctx.outer eq null) true
      else isThisMutable(thisSym)(ctx.outer)
    }
  }
}

