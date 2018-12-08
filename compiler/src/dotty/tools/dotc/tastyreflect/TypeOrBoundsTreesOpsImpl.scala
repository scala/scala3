package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.{Contexts, Types}


trait TypeOrBoundsTreesOpsImpl extends scala.tasty.reflect.TypeOrBoundsTreeOps with CoreImpl {

  // ----- TypeOrBoundsTree ------------------------------------------------

  def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI = new TypeOrBoundsTreeAPI {
    def tpe(implicit ctx: Context): Type = tpt.tpe.stripTypeVar
  }

  // ----- TypeTrees ------------------------------------------------

  def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI = new TypeTreeAPI {
    def pos(implicit ctx: Context): Position = tpt.pos
    def symbol(implicit ctx: Context): Symbol = tpt.symbol
    def tpe(implicit ctx: Context): Type = tpt.tpe.stripTypeVar
  }

  object IsTypeTree extends IsTypeTreeModule {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeTree] =
      if (x.isType) Some(x) else None
    def unapply(termOrTypeTree: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[TypeTree] =
      if (termOrTypeTree.isType) Some(termOrTypeTree) else None
  }

  object TypeTree extends TypeTreeModule with TypeTreeCoreModuleImpl {

    object IsInferred extends IsInferredModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Inferred] = tpt match {
        case tpt: tpd.TypeTree if !tpt.tpe.isInstanceOf[Types.TypeBounds] => Some(tpt)
        case _ => None
      }
    }

    def InferredDeco(x: Inferred): InferredAPI = new InferredAPI {

    }

    object Inferred extends InferredModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Boolean = x match {
        case x @ Trees.TypeTree() => !x.tpe.isInstanceOf[Types.TypeBounds]
        case _ => false
      }
    }

    object IsIdent extends IsIdentModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Ident] = tpt match {
        case tpt: tpd.Ident if tpt.isType => Some(tpt)
        case _ => None
      }
    }

    def IdentDeco(x: Ident): IdentAPI = new IdentAPI {
      def name(implicit ctx: Contexts.Context): String = x.name.toString
    }

    object Ident extends IdentModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[String] = x match {
        case x: tpd.Ident if x.isType => Some(x.name.toString)
        case _ => None
      }
    }

    object IsSelect extends IsSelectModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Select] = tpt match {
        case tpt: tpd.Select if tpt.isType && tpt.qualifier.isTerm  => Some(tpt)
        case _ => None
      }
    }

    def SelectDeco(x: Select): SelectAPI = new SelectAPI {
      def qualifier(implicit ctx: Contexts.Context): Term = x.qualifier
      def name(implicit ctx: Contexts.Context): String = x.name.toString
    }

    object Select extends SelectModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(Term, String)] = x match {
        case x: tpd.Select if x.isType && x.qualifier.isTerm => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object IsProject extends IsProjectModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Project] = tpt match {
        case tpt: tpd.Select if tpt.isType && tpt.qualifier.isType => Some(tpt)
        case _ => None
      }
    }

    def ProjectDeco(x: Project): ProjectAPI = new ProjectAPI {
      def qualifier(implicit ctx: Contexts.Context): TypeTree = x.qualifier
      def name(implicit ctx: Contexts.Context): String = x.name.toString
    }

    object Project extends ProjectModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, String)] = x match {
        case x: tpd.Select if x.isType && x.qualifier.isType => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object IsSingleton extends IsSingletonModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Singleton] = tpt match {
        case tpt: tpd.SingletonTypeTree => Some(tpt)
        case _ => None
      }
    }

    def SingletonDeco(x: Singleton): SingletonAPI = new SingletonAPI {
      def ref(implicit ctx: Contexts.Context): Term = x.ref
    }

    object Singleton extends SingletonModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[Term] = x match {
        case x: tpd.SingletonTypeTree => Some(x.ref)
        case _ => None
      }
    }

    object IsRefined extends IsRefinedModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Refined] = tpt match {
        case tpt: tpd.RefinedTypeTree => Some(tpt)
        case _ => None
      }
    }

    def RefinedDeco(x: Refined): RefinedAPI = new RefinedAPI {
      def tpt(implicit ctx: Contexts.Context): TypeTree = x.tpt
      def refinements(implicit ctx: Contexts.Context): List[Definition] = x.refinements
    }

    object Refined extends RefinedModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] = x match {
        case x: tpd.RefinedTypeTree => Some(x.tpt, x.refinements)
        case _ => None
      }
    }

    object IsApplied extends IsAppliedModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Applied] = tpt match {
        case tpt: tpd.AppliedTypeTree => Some(tpt)
        case _ => None
      }
    }

    def AppliedDeco(x: Applied): AppliedAPI = new AppliedAPI {
      def tpt(implicit ctx: Contexts.Context): TypeTree = x.tpt
      def args(implicit ctx: Contexts.Context): List[TypeOrBoundsTree] = x.args
    }

    object Applied extends AppliedModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])] = x match {
        case x: tpd.AppliedTypeTree => Some(x.tpt, x.args)
        case _ => None
      }
    }

    object IsAnnotated extends IsAnnotatedModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Annotated] = tpt match {
        case tpt: tpd.Annotated => Some(tpt)
        case _ => None
      }
    }

    def AnnotatedDeco(x: Annotated): AnnotatedAPI = new AnnotatedAPI {
      def arg(implicit ctx: Contexts.Context): TypeTree = x.arg
      def annotation(implicit ctx: Contexts.Context): Term = x.annot
    }

    object Annotated extends AnnotatedModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, Term)] = x match {
        case x: tpd.Annotated => Some(x.arg, x.annot)
        case _ => None
      }
    }

    object IsAnd extends IsAndModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[And] = tpt match {
        case tpt: tpd.AndTypeTree => Some(tpt)
        case _ => None
      }
    }

    def AndDeco(x: And): OrAPI = new OrAPI {
      def left(implicit ctx: Contexts.Context): TypeTree = x.left
      def right(implicit ctx: Contexts.Context): TypeTree = x.right
    }

    object And extends AndModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.AndTypeTree => Some(x.left, x.right)
        case _ => None
      }
    }

    object IsOr extends IsOrModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Or] = tpt match {
        case tpt: tpd.OrTypeTree => Some(tpt)
        case _ => None
      }
    }

    def OrDeco(x: Or): OrAPI = new OrAPI {
      def left(implicit ctx: Contexts.Context): TypeTree = x.left
      def right(implicit ctx: Contexts.Context): TypeTree = x.right
    }

    object Or extends OrModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.OrTypeTree => Some(x.left, x.right)
        case _ => None
      }
    }

    object IsMatchType extends IsMatchTypeModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[MatchType] = tpt match {
        case tpt: tpd.MatchTypeTree => Some(tpt)
        case _ => None
      }
    }

    def MatchTypeDeco(x: MatchType): MatchTypeAPI = new MatchTypeAPI {
      def bound(implicit ctx: Contexts.Context): Option[TypeTree] = if (x.bound == tpd.EmptyTree) None else Some(x.bound)
      def selector(implicit ctx: Contexts.Context): TypeTree = x.selector
      def cases(implicit ctx: Contexts.Context): List[CaseDef] = x.cases
    }

    object MatchType extends MatchTypeModule {
      def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[CaseDef])] = x match {
        case x: tpd.MatchTypeTree => Some((if (x.bound == tpd.EmptyTree) None else Some(x.bound), x.selector, x.cases))
        case _ => None
      }
    }

    object IsByName extends IsByNameModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[ByName] = tpt match {
        case tpt: tpd.ByNameTypeTree => Some(tpt)
        case _ => None
      }
    }

    def ByNameDeco(x: ByName): ByNameAPI = new ByNameAPI {
      def result(implicit ctx: Contexts.Context): TypeTree = x.result
    }

    object ByName extends ByNameModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[TypeTree] = x match {
        case x: tpd.ByNameTypeTree => Some(x.result)
        case _ => None
      }
    }

    object IsLambdaTypeTree extends IsLambdaTypeTreeModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[LambdaTypeTree] = tpt match {
        case tpt: tpd.LambdaTypeTree => Some(tpt)
        case _ => None
      }
    }

    def LambdaTypeTreeDeco(x: LambdaTypeTree): LambdaTypeTreeAPI = new LambdaTypeTreeAPI {
      def tparams(implicit ctx: Contexts.Context): List[TypeDef] = x.tparams
      def body(implicit ctx: Contexts.Context): TypeOrBoundsTree = x.body
    }

    object LambdaTypeTree extends LambdaTypeTreeModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(List[TypeDef], TypeOrBoundsTree)] = x match {
        case Trees.LambdaTypeTree(tparams, body) => Some((tparams, body))
        case _ => None
      }
    }

    object IsBind extends IsBindModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Bind] = tpt match {
        case tpt: tpd.Bind if tpt.name.isTypeName => Some(tpt)
        case _ => None
      }
    }

    def BindDeco(x: Bind): BindAPI = new BindAPI {
      def name(implicit ctx: Contexts.Context): String = x.name.toString
      def body(implicit ctx: Contexts.Context): TypeOrBoundsTree = x.body
    }

    object Bind extends BindModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree)] = x match {
        case x: tpd.Bind if x.name.isTypeName => Some((x.name.toString, x.body))
        case _ => None
      }
    }

    object IsBlock extends IsBlockModule {
      def unapply(tpt: TypeOrBoundsTree)(implicit ctx: Context): Option[Block] = tpt match {
        case tpt: tpd.Block => Some(tpt)
        case _ => None
      }
    }

    def BlockDeco(x: Block): BlockAPI = new BlockAPI {
      def aliases(implicit ctx: Contexts.Context): List[TypeDef] = x.stats.map { case alias: TypeDef => alias }
      def tpt(implicit ctx: Contexts.Context): TypeTree = x.expr
    }

    object Block extends BlockModule {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)] = x match {
        case x: tpd.Block => Some((x.stats.map { case alias: TypeDef => alias }, x.expr))
        case _ => None
      }
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  def TypeBoundsTreeDeco(bounds: TypeBoundsTree): TypeBoundsTreeAPI = new TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds = bounds.tpe.asInstanceOf[Types.TypeBounds]
    def low(implicit ctx: Context): TypeTree = bounds.lo
    def hi(implicit ctx: Context): TypeTree = bounds.hi
  }

  object IsTypeBoundsTree extends IsTypeBoundsTreeModule {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[TypeBoundsTree] = x match {
      case x: tpd.TypeBoundsTree => Some(x)
      case x @ Trees.TypeTree() =>
        // TODO only enums generate this kind of type bounds. Is this possible without enums? If not generate tpd.TypeBoundsTree for enums instead
        x.tpe match {
          case tpe: Types.TypeBounds =>
            Some(tpd.TypeBoundsTree(tpd.TypeTree(tpe.lo).withPos(x.pos), tpd.TypeTree(tpe.hi).withPos(x.pos)))
          case _ => None
        }
      case _ => None
    }
  }

  object TypeBoundsTree extends TypeBoundsTreeModule {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
      case IsTypeBoundsTree(x) => Some((x.lo, x.hi))
      case _ => None
    }
  }

  object WildcardTypeTree extends WildcardTypeTreeModule {
    def unapply(x: TypeOrBoundsTree)(implicit ctx: Context): Boolean = x match {
      case Trees.Ident(nme.WILDCARD) => x.tpe.isInstanceOf[Types.TypeBounds]
      case _ => false
    }
  }

  def typeTreeAsParent(typeTree: TypeTree): TermOrTypeTree = typeTree
}
