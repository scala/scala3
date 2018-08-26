package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types

import scala.tasty.util.Show

trait TypeOrBoundsTreesOpsImpl extends scala.tasty.reflect.TypeOrBoundsTreeOps with TastyCoreImpl {

  // ----- TypeOrBoundsTree ------------------------------------------------

  def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI = new TypeOrBoundsTreeAPI {
    def tpe(implicit ctx: Context): Type = tpt.tpe.stripTypeVar
  }

  // ----- TypeTrees ------------------------------------------------

  def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI = new TypeTreeAPI {
    def pos(implicit ctx: Context): Position = tpt.pos
    def tpe(implicit ctx: Context): Types.Type = tpt.tpe.stripTypeVar
  }

  object IsTypeTree extends IsTypeTreeExtractor {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree] =
      if (x.isType) Some(x) else None
    def unapply(x: Parent)(implicit ctx: Context, dummy: DummyImplicit): Option[TypeTree] =
      if (x.isType) Some(x) else None
  }

  object TypeTree extends TypeTreeModule {

    object Synthetic extends SyntheticExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Boolean = x match {
        case x @ Trees.TypeTree() => !x.tpe.isInstanceOf[Types.TypeBounds]
        case _ => false
      }
    }

    object TypeIdent extends TypeIdentExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[String] = x match {
        case x: tpd.Ident if x.isType => Some(x.name.toString)
        case _ => None
      }
    }

    object TermSelect extends TermSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(Term, String)] = x match {
        case x: tpd.Select if x.isType && x.qualifier.isTerm => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object TypeSelect extends TypeSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, String)] = x match {
        case x: tpd.Select if x.isType && x.qualifier.isType => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object Singleton extends SingletonExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[Term] = x match {
        case x: tpd.SingletonTypeTree => Some(x.ref)
        case _ => None
      }
    }

    object Refined extends RefinedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] = x match {
        case x: tpd.RefinedTypeTree => Some(x.tpt, x.refinements)
        case _ => None
      }
    }

    object Applied extends AppliedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])] = x match {
        case x: tpd.AppliedTypeTree => Some(x.tpt, x.args)
        case _ => None
      }
    }

    object Annotated extends AnnotatedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, Term)] = x match {
        case x: tpd.Annotated => Some(x.arg, x.annot)
        case _ => None
      }
    }

    object And extends AndExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.AndTypeTree => Some(x.left, x.right)
        case _ => None
      }
    }

    object Or extends OrExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.OrTypeTree => Some(x.left, x.right)
        case _ => None
      }
    }

    object ByName extends ByNameExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[TypeTree] = x match {
        case x: tpd.ByNameTypeTree => Some(x.result)
        case _ => None
      }
    }

    object TypeLambdaTree extends TypeLambdaTreeExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)] = x match {
        case Trees.LambdaTypeTree(tparams, body) => Some((tparams, body))
        case _ => None
      }
    }

    object Bind extends BindExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(String, TypeBoundsTree)] = x match {
        case x: tpd.Bind if x.name.isTypeName => Some((x.name.toString, x.body))
        case _ => None
      }
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  def TypeBoundsTreeDeco(bounds: TypeBoundsTree): TypeBoundsTreeAPI = new TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds = bounds.tpe.asInstanceOf[Types.TypeBounds]
    def low(implicit ctx: Context): TypeTree = bounds.asInstanceOf[tpd.TypeBoundsTree].lo
    def hi(implicit ctx: Context): TypeTree = bounds.asInstanceOf[tpd.TypeBoundsTree].hi
  }

  object IsTypeBoundsTree extends IsTypeBoundsTreeExtractor {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree] = x match {
      case x: tpd.TypeBoundsTree => Some(x)
      case _ => None
    }
  }

  object TypeBoundsTree extends TypeBoundsTreeExtractor {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
      case x: tpd.TypeBoundsTree => Some(x.lo, x.hi)
      case _ => None
    }
  }

  object SyntheticBounds extends SyntheticBoundsExtractor {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Boolean = x match {
      case x @ Trees.TypeTree() => x.tpe.isInstanceOf[Types.TypeBounds]
      case Trees.Ident(nme.WILDCARD) => x.tpe.isInstanceOf[Types.TypeBounds]
      case _ => false
    }
  }

  def typeTreeAsParent(typeTree: TypeTree): Parent = typeTree
}
