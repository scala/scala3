package dotty.tools.dotc

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd
import core._
import Types._, Symbols._, Annotations._, Contexts._
import dotty.tools.dotc.config.Printers.{noPrinter, Printer}
import dotty.tools.dotc.transform.OverridingPairs
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, MiniPhase, TransformerInfo, MiniPhaseTransform}
import dotty.tools.dotc.typer.ProtoTypes.FunProto
import dotty.tools.dotc.typer._
import ErrorReporting._
import Decorators._
import Flags._
import util.Attachment


object Reim {
  val reim: Printer = noPrinter

  sealed abstract class BoundSpec
  case object Hi extends BoundSpec
  case object Lo extends BoundSpec
  case object Inferred extends BoundSpec

  implicit class TypeDecorator(val tpe: Type) extends AnyVal {
    def withAnnotation(cls: ClassSymbol)(implicit ctx: Context) =
      AnnotatedType(Annotation(cls, Nil), tpe.removeAnnotations)

    def removeAnnotations(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType if isAnnotation(tpe.annot.symbol) => tpe.tpe.removeAnnotations
      case tpe: AnnotatedType => tpe.derivedAnnotatedType(tpe.annot, tpe.tpe.removeAnnotations)
      case _ => tpe
    }

    def directReimAnnotation(implicit ctx: Context): Option[ClassSymbol] = tpe match {
      case tpe: AnnotatedType =>
        if(isAnnotation(tpe.annot.symbol)) Some(tpe.annot.symbol.asInstanceOf[ClassSymbol]) else tpe.tpe.directReimAnnotation
      case _ => None
    }

    def reimAnnotation(bound: BoundSpec = Inferred)(implicit ctx: Context): Option[ClassSymbol] = {
      def ifConsistent(tpe: Type): Option[ClassSymbol] = {
        val hi = tpe.reimAnnotation(Hi).get
        val lo = tpe.reimAnnotation(Lo).get
        if(hi == lo) Some(hi) else None
      }
      def anyAnnot: Option[ClassSymbol] = {
        bound match {
          case Inferred => None
          case Hi => Some(defn.ReadOnlyAnnot)
          case Lo => Some(defn.MutableAnnot)
        }
      }
      tpe.directReimAnnotation.orElse(tpe.stripAnnots match {
        case tpe: TypeRef => tpe.underlying.reimAnnotation(bound)
        case tpe: TermRef =>
          val symbol = tpe.symbol
          if(symbol is Method) symbol.info.reimAnnotation(bound)
          else bound match {
            case Inferred =>
              val preAnnot = tpe.prefix.reimAnnotation(Inferred)
                .getOrElse(tpe.prefix.reimAnnotation(Hi).get)
              ifConsistent(symbol.info).map(_.lub(preAnnot))
            case _ =>
              val preAnnot = tpe.prefix.reimAnnotation(bound).get
              val symAnnot = symbol.info.reimAnnotation(bound).get
              Some(symAnnot.lub(preAnnot))
          }
        case tpe: ThisType => Some(getAnnotationOfThis(tpe.cls))
        case tpe: SuperType => tpe.thistpe.reimAnnotation(bound)
        case tpe: ConstantType => Some(defn.MutableAnnot)
        case tpe: MethodParam => tpe.underlying.reimAnnotation(bound)
        // TODO: SkolemType
        case tpe: PolyParam => ctx.typerState.constraint.fullBounds(tpe).reimAnnotation(bound)
        case tpe: RefinedType => tpe.parent.reimAnnotation(bound)
        case tpe: TypeBounds => bound match {
          case Inferred => ifConsistent(tpe)
          case Lo => tpe.lo.reimAnnotation(Lo)
          case Hi => tpe.hi.reimAnnotation(Hi)
        }
        case tpe: TypeAlias => tpe.underlying.reimAnnotation(bound)
        case tpe: ExprType => tpe.underlying.reimAnnotation(bound)
        case tpe: TypeVar => tpe.underlying.reimAnnotation(bound)
        case tpe: AndType => bound match {
          case Inferred => ifConsistent(tpe)
          case _ =>
            Some(tpe.tp1.reimAnnotation(bound).get.lub(tpe.tp2.reimAnnotation(bound).get))
        }
        case tpe: OrType => bound match {
          case Inferred => ifConsistent(tpe)
          case _ =>
            Some(tpe.tp1.reimAnnotation(bound).get.glb(tpe.tp2.reimAnnotation(bound).get))
        }
        case tpe: ClassInfo => Some(defn.MutableAnnot)
        case NoPrefix => Some(defn.MutableAnnot)
        case tpe: ErrorType => Some(defn.MutableAnnot)
        case tpe: WildcardType => tpe.optBounds match {
          case info: TypeBounds => info.reimAnnotation(bound)
          case NoType => anyAnnot
        }
        case tpe: ImportType => anyAnnot
        case tpe: MethodType => anyAnnot
        case tpe: PolyType => anyAnnot
        case NoType => anyAnnot
      })
    }
  }

  implicit class TreeDecorator(val tree: tpd.Tree) extends AnyVal {
    def withAnnotation(cls: ClassSymbol)(implicit ctx: Context) = tree.withType(tree.tpe.withAnnotation(cls))
  }

  implicit class SymbolDecorator(val symbol: Symbol) extends AnyVal {
    def reimAnnotation(implicit ctx: Context): ClassSymbol =
      symbol.annotations.map(_.symbol).filter(isAnnotation(_))
        .headOption.getOrElse(defn.MutableAnnot).asInstanceOf[ClassSymbol]

    def isVal(implicit ctx: Context): Boolean =
      !(symbol.isClass || symbol.isType || (symbol is Method))
  }

  implicit class ClassSymbolDecorator(val symbol: ClassSymbol) extends AnyVal {
    def <:<(other: ClassSymbol)(implicit ctx: Context) = {
      assert(isAnnotation(symbol))
      assert(isAnnotation(other))
      symbol == other || symbol == defn.MutableAnnot || other == defn.ReadOnlyAnnot
    }

    def lub(other: ClassSymbol)(implicit ctx: Context): ClassSymbol =
      if(symbol <:< other) other else symbol
    def glb(other: ClassSymbol)(implicit ctx: Context): ClassSymbol =
      if(symbol <:< other) symbol else other
  }

  private def isAnnotation(symbol: Symbol)(implicit ctx: Context) =
    (symbol == defn.MutableAnnot) || (symbol == defn.ReadOnlyAnnot) || (symbol == defn.PolyReadAnnot)

  val PolyReadKey = new Attachment.Key[PolyReadAttachments]
  sealed abstract class PolyReadAttachments
  object Arg extends PolyReadAttachments
  object PolyReadNeedsToBeReadOnly extends PolyReadAttachments
  object PolyReadNeedsToBePolyRead extends PolyReadAttachments

  class ReimTyper extends Typer {

    private def explicitAnnotIfNeeded(tree: tpd.Tree)(implicit ctx: Context) = {
      val reimAnnotation = tree.tpe.reimAnnotation(Inferred)
      if (reimAnnotation.exists(_ != tree.tpe.directReimAnnotation.getOrElse(defn.MutableAnnot)))
        tree.withType(tree.tpe.withAnnotation(reimAnnotation.get))
      else tree
    }

    private def checkValDefAnnot(tree: tpd.Tree)(implicit ctx: Context) = tree match {
      case tree: tpd.ValDef =>
        val annot = tree.tpe.reimAnnotation(Lo).get
        if(annot == defn.MutableAnnot) tree.withType(tree.tpe.withAnnotation(defn.PolyReadAnnot))
        else tree
      case _ => tree
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      /** Perform viewpoint adaptation on a method call. */
      def viewPointAdapt(tree: tpd.Tree): tpd.Tree =
        if ((tree.symbol is Method) && (!tree.isInstanceOf[tpd.DefTree]) && (tree.tpe.reimAnnotation(Hi).get == defn.PolyReadAnnot)) {
          def fun(tree: tpd.Tree): tpd.Tree = tree match {
            case _: tpd.Ident | _: tpd.Select => tree
            case tree: tpd.Apply => fun(tree.fun)
            case tree: tpd.TypeApply => fun(tree.fun)
            case tree: tpd.Typed => fun(tree.expr)
          }
          val receiverType = fun(tree).tpe.stripAnnots.asInstanceOf[TermRef].prefix
          var replacement = defn.MutableAnnot
          // is the receiver parameter polyread and is the argument receiver non-mutable?
          if(tree.symbol.reimAnnotation == defn.PolyReadAnnot) replacement = receiverType.reimAnnotation(Hi).get
          reim.println(s"reimAnnotation of ${tree.symbol} is ${tree.symbol.reimAnnotation}")
          // do any polyread parameters have non-mutable arguments?
          original match {
            case original: untpd.Apply => for(arg <- original.args) {
              val attachment = arg.removeAttachment(PolyReadKey)
              if(attachment.contains(PolyReadNeedsToBeReadOnly)) replacement = replacement.lub(defn.ReadOnlyAnnot)
              if(attachment.contains(PolyReadNeedsToBePolyRead)) replacement = replacement.lub(defn.PolyReadAnnot)
            }
            case _ =>
          }

          tree.withAnnotation(replacement)
        } else tree


      pt match {
        case proto: FunProto => proto.args.foreach(arg => arg.putAttachment(PolyReadKey, Arg))
        case _ =>
      }

      val tree = checkValDefAnnot(explicitAnnotIfNeeded(viewPointAdapt(super.adapt(tree0, pt, original))))

      val dontCheck = (
           pt == WildcardType
        || !tree.tpe.exists
        || pt.isInstanceOf[ProtoType]
        || tree.tpe <:< defn.AnyValType
        )

      if(dontCheck) tree else {
        val ptAnnot = pt.reimAnnotation(Lo).get
        val treeAnnot = tree.tpe.reimAnnotation(Hi).get
        if(ptAnnot == defn.PolyReadAnnot && original.removeAttachment(PolyReadKey).contains(Arg)) {
          if(treeAnnot == defn.ReadOnlyAnnot) original.putAttachment(PolyReadKey, PolyReadNeedsToBeReadOnly)
          if(treeAnnot == defn.PolyReadAnnot) original.putAttachment(PolyReadKey, PolyReadNeedsToBePolyRead)
          tree
        } else if (!((tree.tpe == pt) || (treeAnnot <:< ptAnnot))) {
          err.typeMismatch(tree, pt)
        } else tree
      }
    }
  }

  def getAnnotationOfThis(thisSym: Symbol)(implicit ctx: Context): ClassSymbol = {
    val owner = ctx.owner
    assert(owner ne null)
    if (!owner.exists) {
      // We have walked the owner chain and not found the symbol. This can happen in two cases:
      // 1. `thisSym` is a module class (i.e. a globally static object) referenced from outside.
      // 2. `thisSym` appears in a declaration of a self-type, which gets type-checked *outside* the
      //     scope of its defining class.
      // I don't know of a condition to assert for case 2 (we would need the `Tree`), so we have
      // to comment the assertion for case 1 out.
//      assert(thisSym is ModuleClass)
      defn.MutableAnnot
    } else if (owner == thisSym || ((owner is Flags.Method) && (owner.owner == thisSym))) {
      if (owner.isCompleted) owner.reimAnnotation
      else {
        // Evil hack: We only need the annotations, but the symbol is not yet completed.
        // So we reach into the completer to get the corresponding tree, and look at the
        // annotations on the tree.
        val typer = ctx.typer
        val completer = owner.completer.asInstanceOf[typer.Completer]
        val tree = completer.original.asInstanceOf[untpd.MemberDef]
        val annots = untpd.modsDeco(tree).mods.annotations.mapconserve(typer.typedAnnotation(_))
        def annot(annots: List[tpd.Tree]): ClassSymbol = annots match {
          case Nil => defn.MutableAnnot
          case head :: tail =>
            val symbol = head.symbol
            if (isAnnotation(symbol)) symbol.asInstanceOf[ClassSymbol] else annot(tail)
        }
        annot(annots)
      }
    } else getAnnotationOfThis(thisSym)(ctx.outer)
  }


  /** This phase checks that the prefix of the target of any assignment is mutable. */
  class ReimPhase extends MiniPhaseTransform {
    override def phaseName: String = "reim"

    override def transformAssign(tree: tpd.Assign)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
      tree.lhs match {
        case _: tpd.Select | _: tpd.Ident =>
          val annot = tree.lhs.tpe.stripAnnots match {
            case tr: TermRef => tr.prefix.reimAnnotation(Hi).get
          }
          if(annot == defn.MutableAnnot) tree
          else errorTree(tree, s"assignment to field of @${annot.name} reference")
      }
  }

  class ReimTypeComparer(initctx: Context) extends TypeComparer(initctx) {
    override def isSubType(tp1: Type, tp2: Type): Boolean = super.isSubType(tp1, tp2) && (
      !tp1.widenExpr.stripAnnots.isValueType || !tp2.widenExpr.stripAnnots.isValueType || tp1 == tp2 || {
      val annot1 = tp1.widenExpr.reimAnnotation(Hi).get
      val annot2 = tp2.widenExpr.reimAnnotation(Lo).get
      annot1 <:< annot2
    })

    override def copyIn(ctx: Context): TypeComparer = new ReimTypeComparer(ctx)
  }

  /** This phase runs the regular Scala RefChecks with the Reim type comparer to enforce necessary
    * subtyping relationships between symbols.
    */
  class ReimRefChecks extends RefChecks {
    override def run(implicit ctx: Context): Unit = super.run(ctx.fresh.setTypeComparerFn(c => new ReimTypeComparer(c)))
    override def phaseName: String = "reimrefchecks"
  }

  /** This phase checks addition Reim-specific subtyping relationships between symbols.
    */
  class ReimRefChecks2 extends MiniPhase { thisTransformer =>
    val treeTransform = new TreeTransform {

      private def checkReceiver(fun: tpd.Tree, tree: tpd.Tree)(implicit ctx: Context) = {
        val receiverType = tree.tpe.stripAnnots.asInstanceOf[TermRef].prefix
        val receiverAnnot = receiverType.reimAnnotation(Hi).get
        val methodSym = tree.symbol
        val methodAnnot = methodSym.reimAnnotation
        if(methodAnnot == defn.MutableAnnot && receiverAnnot != defn.MutableAnnot) {
          errorTree(tree, s"call of $methodSym taking @${methodAnnot.name} this on @${receiverAnnot.name} receiver")
        } else tree
      }

      override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
        if(tree.symbol is Method) checkReceiver(tree, tree) else tree

      override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
        if(tree.symbol is Method) checkReceiver(tree, tree) else tree

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        def adjustedAnnotation(symbol: Symbol): ClassSymbol =
          if(symbol is Method) symbol.reimAnnotation
          else defn.PolyReadAnnot
        val cursor = new OverridingPairs.Cursor(ctx.owner)
        while (cursor.hasNext) {
          val overridingAnnot = adjustedAnnotation(cursor.overriding)
          val overriddenAnnot = adjustedAnnotation(cursor.overridden)
          if(!(overriddenAnnot <:< overridingAnnot))
            ctx.error(s"${cursor.overriding} with @${overridingAnnot.name} this cannot override ${cursor.overridden} with @${overriddenAnnot.name} this", cursor.overriding.pos)
          // If a val overrides a method, the method's return type must be at least @polyread, since the val will be viewpoint-adapted.
          if(cursor.overriding.isVal && !(defn.PolyReadAnnot <:< cursor.overridden.info.finalResultType.reimAnnotation(Lo).get))
            ctx.error(s"${cursor.overriding} with @${overridingAnnot.name} cannot override ${cursor.overridden} that could return @mutable", cursor.overriding.pos)
          cursor.next()
        }
        tree
      }

      def phase = thisTransformer
    }

    def phaseName: String = "reimrefchecks2"
  }
}
