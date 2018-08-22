package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer
import dotty.tools.dotc.util.{Positions, SourcePosition}

import scala.quoted
import scala.reflect.ClassTag
import scala.tasty.util.{Show, ShowExtractors, ShowSourceCode}

class TastyImpl(val rootContext: Contexts.Context) extends scala.tasty.Tasty { self =>

  // ===== Quotes ===================================================

  def QuotedExprDeco[T](x: quoted.Expr[T]): QuotedExprAPI = new QuotedExprAPI {
    def toTasty(implicit ctx: Context): Term = PickledQuotes.quotedExprToTree(x)
  }

  def QuotedTypeDeco[T](x: quoted.Type[T]): QuotedTypeAPI = new QuotedTypeAPI {
    def toTasty(implicit ctx: Context): TypeTree = PickledQuotes.quotedTypeToTree(x)
  }

  // ===== Show =====================================================

  def defaultShow: Show[this.type] = showExtractors

  def showExtractors: Show[this.type] = new ShowExtractors(this)

  def showSourceCode: Show[this.type] = new ShowSourceCode(this)

  // ===== Contexts =================================================

  type Context = Contexts.Context

  def ContextDeco(ctx: Context): ContextAPI = new ContextAPI {
    def owner: Definition = FromSymbol.definition(ctx.owner)(ctx)

    def source: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
  }

  def rootPosition: SourcePosition = SourcePosition(rootContext.source, Positions.NoPosition)

  // ===== Id =======================================================

  type Id = untpd.Ident

  def IdDeco(id: Id): IdAPI = new IdAPI {
    def pos(implicit ctx: Context): Position = id.pos
    def name(implicit ctx: Context): String = id.name.toString
  }

  def idClassTag: ClassTag[Id] = implicitly[ClassTag[Id]]

  object Id extends IdExtractor {
    def unapply(x: Id): Option[String] = x match {
      case x: untpd.Ident => Some(x.name.toString) // TODO how to make sure it is not a Ident or TypeIdent? Check x.tpe?
      case _ => None
    }
  }

  // ===== Trees ====================================================

  type Tree = tpd.Tree

  def TreeDeco(tree: Tree): TreeAPI = new TreeAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showTree(tree)
    def pos(implicit ctx: Context): Position = tree.pos
  }

  type PackageClause = tpd.PackageDef

  def packageClauseClassTag: ClassTag[PackageClause] = implicitly[ClassTag[PackageClause]]

  object PackageClause extends PackageClauseExtractor {
    def unapply(x: PackageClause)(implicit ctx: Context): Option[(Term, List[Tree])] = x match {
      case x: tpd.PackageDef @unchecked => Some((x.pid, x.stats))
      case _ => None
    }
  }

  def PackageClauseDeco(pack: PackageClause): PackageClauseAPI = new PackageClauseAPI {
    def definition(implicit ctx: Context): Definition = FromSymbol.packageDef(pack.symbol)
  }

  // ----- Statements -----------------------------------------------

  type Statement = tpd.Tree

  type Import = tpd.Import

  def importClassTag: ClassTag[Import] = implicitly[ClassTag[Import]]

  object Import extends ImportExtractor {
    def unapply(x: Import)(implicit ctx: Context): Option[(Term, List[ImportSelector])] = x match {
      case x: tpd.Import @unchecked => Some((x.expr, x.selectors))
      case _ => None
    }
  }

  def ImportDeco(imp: Import): ImportAPI = new ImportAPI {
    def expr(implicit ctx: Context): Tree = imp.expr
    def selector(implicit ctx: Context): List[ImportSelector] = imp.selectors
  }

  type ImportSelector = untpd.Tree

  def importSelectorClassTag: ClassTag[ImportSelector] = implicitly[ClassTag[ImportSelector]]

  object SimpleSelector extends SimpleSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id] = x match {
      case x: untpd.Ident => Some(x) // TODO make sure it will not match other idents
      case _ => None
    }
  }

  object RenameSelector extends RenameSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[(Id, Id)] = x match {
      case Trees.Thicket((id1: untpd.Ident) :: (id2: untpd.Ident) :: Nil) if id2.name != nme.WILDCARD => Some(id1, id2)
      case _ => None
    }
  }

  object OmitSelector extends OmitSelectorExtractor {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id] = x match {
      case Trees.Thicket((id: untpd.Ident) :: Trees.Ident(nme.WILDCARD) :: Nil) => Some(id)
      case _ => None
    }
  }

  // ----- Definitions ----------------------------------------------

  type Definition = tpd.Tree

  object Definition extends DefinitionExtractor {
    def unapply(x: Definition)(implicit ctx: Context): Boolean =
      x.isInstanceOf[Trees.MemberDef[_]]
  }

  def DefinitionDeco(definition: Definition): DefinitionAPI = new DefinitionAPI {

    def name(implicit ctx: Context): String = definition.symbol.name.toString

    def owner(implicit ctx: Context): Definition = FromSymbol.definition(definition.symbol.owner)

    def flags(implicit ctx: Context): FlagSet =
      new FlagSet(definition.symbol.flags)

    def privateWithin(implicit ctx: Context): Option[Type] = {
      val within = definition.symbol.privateWithin
      if (within.exists && !definition.symbol.is(core.Flags.Protected)) Some(within.typeRef)
      else None
    }

    def protectedWithin(implicit ctx: Context): Option[Type] = {
      val within = definition.symbol.privateWithin
      if (within.exists && definition.symbol.is(core.Flags.Protected)) Some(within.typeRef)
      else None
    }

    def annots(implicit ctx: Context): List[Term] = {
      definition.symbol.annotations.flatMap {
        case _: core.Annotations.LazyBodyAnnotation => Nil
        case annot => annot.tree :: Nil
      }
    }

    def localContext(implicit ctx: Context): Context =
      if (definition.hasType && definition.symbol.exists) ctx.withOwner(definition.symbol)
      else ctx
  }

  def definitionClassTag: ClassTag[Definition] = implicitly[ClassTag[Definition]]

  // ClassDef

  type ClassDef = tpd.TypeDef

  def classDefClassTag: ClassTag[ClassDef] = implicitly[ClassTag[ClassDef]]

  object ClassDef extends ClassDefExtractor {
    def unapply(x: ClassDef)(implicit ctx: Context): Option[(String, DefDef, List[Parent],  Option[ValDef], List[Statement])] = x match {
      case x: tpd.TypeDef @unchecked if x.isClassDef =>
        val deco = ClassDefDeco(x)
        Some((x.name.toString, deco.constructor, deco.parents, deco.self, deco.body))
      case _ => None
    }
  }

  def ClassDefDeco(cdef: ClassDef): ClassDefAPI = new ClassDefAPI {
    private[this] val rhs = cdef.rhs.asInstanceOf[tpd.Template]
    def constructor(implicit ctx: Context): DefDef = rhs.constr
    def parents(implicit ctx: Context): List[tpd.Tree] = rhs.parents
    def self(implicit ctx: Context): Option[tpd.ValDef] = optional(rhs.self)
    def body(implicit ctx: Context): List[tpd.Tree] = rhs.body
  }

  // DefDef

  type DefDef = tpd.DefDef

  def defDefClassTag: ClassTag[DefDef] = implicitly[ClassTag[DefDef]]

  object DefDef extends DefDefExtractor {
    def unapply(x: DefDef)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])] = x match {
      case x: tpd.DefDef @unchecked =>
        Some((x.name.toString, x.tparams, x.vparamss, x.tpt, optional(x.rhs)))
      case _ => None
    }
  }

  def DefDefDeco(ddef: DefDef): DefDefAPI = new DefDefAPI {
    def typeParams(implicit ctx: Context): List[TypeDef] = ddef.tparams
    def paramss(implicit ctx: Context): List[List[ValDef]] = ddef.vparamss
    def returnTpt(implicit ctx: Context): TypeTree = ddef.tpt
    def rhs(implicit ctx: Context): Option[Tree] = optional(ddef.rhs)
  }

  // ValDef

  type ValDef = tpd.ValDef

  def valDefClassTag: ClassTag[ValDef] = implicitly[ClassTag[ValDef]]

  object ValDef extends ValDefExtractor {
    def unapply(x: ValDef)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])] = x match {
      case x: tpd.ValDef @unchecked =>
        Some((x.name.toString, x.tpt, optional(x.rhs)))
      case _ => None
    }
  }

  def ValDefDeco(vdef: ValDef): ValDefAPI = new ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree = vdef.tpt
    def rhs(implicit ctx: Context): Option[Tree] = optional(vdef.rhs)
  }

  // TypeDef

  type TypeDef = tpd.TypeDef

  def typeDefClassTag: ClassTag[TypeDef] = implicitly[ClassTag[TypeDef]]

  object TypeDef extends TypeDefExtractor {
    def unapply(x: TypeDef)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)] = x match {
      case x: tpd.TypeDef @unchecked if !x.symbol.isClass => Some((x.name.toString, x.rhs))
      case _ => None
    }
  }

  def TypeDefDeco(tdef: TypeDef): TypeDefAPI = new TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree = tdef.rhs
  }

  // PackageDef

  type PackageDef = PackageDefinition

  def PackageDefDeco(pdef: PackageDef): PackageDefAPI = new PackageDefAPI {

    def owner(implicit ctx: Context): PackageDefinition = FromSymbol.packageDef(pdef.symbol.owner)

    def members(implicit ctx: Context): List[Statement] = {
      if (pdef.symbol.is(core.Flags.JavaDefined)) Nil // FIXME should also support java packages
      else pdef.symbol.info.decls.iterator.map(FromSymbol.definition).toList
    }
  }

  def packageDefClassTag: ClassTag[PackageDef] = implicitly[ClassTag[PackageDef]]

  object PackageDef extends PackageDefExtractor {
    def unapply(x: PackageDef)(implicit ctx: Context): Option[(String, PackageDef)] = x match {
      case x: PackageDefinition =>
        Some((x.symbol.name.toString, FromSymbol.packageDef(x.symbol.owner)))
      case _ => None
    }
  }

  // ----- Parents --------------------------------------------------

  type Parent = tpd.Tree

  // ----- Terms ----------------------------------------------------

  type Term = tpd.Tree

  def TermDeco(term: Term): TermAPI = new TermAPI {

    def pos(implicit ctx: Context): Position = term.pos

    def tpe(implicit ctx: Context): Types.Type = term.tpe

    def toExpr[T: quoted.Type](implicit ctx: Context): quoted.Expr[T] = {
      typecheck(ctx)
      new quoted.Exprs.TastyTreeExpr(term).asInstanceOf[quoted.Expr[T]]
    }

    private def typecheck[T: quoted.Type](ctx: Contexts.Context): Unit = {
      implicit val ctx0: Contexts.FreshContext = ctx.fresh
      ctx0.setTyperState(ctx0.typerState.fresh())
      ctx0.typerState.setReporter(new Reporter {
        def doReport(m: MessageContainer)(implicit ctx: Contexts.Context): Unit = ()
      })
      val tp = QuotedTypeDeco(implicitly[quoted.Type[T]]).toTasty
      ctx0.typer.typed(term, tp.tpe)
      if (ctx0.reporter.hasErrors) {
        val stack = new Exception().getStackTrace
        def filter(elem: StackTraceElement) =
          elem.getClassName.startsWith("dotty.tools.dotc.tasty.TastyImpl") ||
          !elem.getClassName.startsWith("dotty.tools.dotc")
        throw new scala.tasty.TastyTypecheckError(
          s"""Error during tasty reflection while typing term
             |term: ${term.show}
             |with expected type: ${tp.tpe.show}
             |
             |  ${stack.takeWhile(filter).mkString("\n  ")}
           """.stripMargin
        )
      }
    }
  }

  def termClassTag: ClassTag[Term] = implicitly[ClassTag[Term]]

  object Term extends TermModule {

    def unapply(x: Term)(implicit ctx: Context): Boolean = x.isTerm

    object Ident extends IdentExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[String] = x match {
        case x: tpd.Ident @unchecked if x.isTerm => Some(x.name.show)
        case _ => None
      }
    }

    object Select extends SelectExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, String, Option[Signature])] = x match {
        case x: tpd.Select @unchecked if x.isTerm =>
          val sig =
            if (x.symbol.signature == core.Signature.NotAMethod) None
            else Some(x.symbol.signature)
          Some((x.qualifier, x.name.toString, sig))
        case _ => None
      }
    }

    object Literal extends LiteralExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[Constant] = x match {
        case Trees.Literal(const) => Some(const)
        case _ => None
      }
    }

    object This extends ThisExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[Option[Id]] = x match {
        case Trees.This(qual) => Some(optional(qual))
        case _ => None
      }
    }

    object New extends NewExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[TypeTree] = x match {
        case x: tpd.New @unchecked => Some(x.tpt)
        case _ => None
      }
    }

    object NamedArg extends NamedArgExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(String, Term)] = x match {
        case x: tpd.NamedArg @unchecked if x.name.isInstanceOf[Names.TermName] => Some((x.name.toString, x.arg))
        case _ => None
      }
    }

    object Apply extends ApplyExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[Term])] = x match {
        case x: tpd.Apply @unchecked => Some((x.fun, x.args))
        case _ => None
      }
    }

    object TypeApply extends TypeApplyExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[TypeTree])] = x match {
        case x: tpd.TypeApply @unchecked => Some((x.fun, x.args))
        case _ => None
      }
    }

    object Super extends SuperExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[Id])] = x match {
        case x: tpd.Super @unchecked => Some((x.qual, if (x.mix.isEmpty) None else Some(x.mix)))
        case _ => None
      }
    }

    object Typed extends TypedExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, TypeTree)] = x match {
        case x: tpd.Typed @unchecked => Some((x.expr, x.tpt))
        case _ => None
      }
    }

    object Assign extends AssignExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)] = x match {
        case x: tpd.Assign @unchecked => Some((x.lhs, x.rhs))
        case _ => None
      }
    }

    object Block extends BlockExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(List[Statement], Term)] = normalizedLoops(x) match {
        case Trees.Block(_, expr) if expr.symbol.is(Flags.Label) => None // while or doWhile loops
        case Trees.Block(stats, expr) => Some((stats, expr))
        case _ => None
      }
      /** Normalizes non Blocks.
       *  i) Put `while` and `doWhile` loops in their own blocks: `{ def while$() = ...; while$() }`
       *  ii) Put closures in their own blocks: `{ def anon$() = ...; closure(anon$, ...) }`
       */
      private def normalizedLoops(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
        case block: tpd.Block if block.stats.size > 1 =>
          def normalizeInnerLoops(stats: List[tpd.Tree]): List[tpd.Tree] = stats match {
            case (x: tpd.DefDef) :: y :: xs if needsNormalization(y) =>
              tpd.Block(x :: Nil, y) :: normalizeInnerLoops(xs)
            case x :: xs => x :: normalizeInnerLoops(xs)
            case Nil => Nil
          }
          if (needsNormalization(block.expr)) {
            val stats1 = normalizeInnerLoops(block.stats.init)
            val normalLoop = tpd.Block(block.stats.last :: Nil, block.expr)
            tpd.Block(stats1, normalLoop)
          } else {
            val stats1 = normalizeInnerLoops(block.stats)
            tpd.cpy.Block(block)(stats1, block.expr)
          }
        case _ => tree
      }

      /** If it is the second statement of a loop or a closure. See: `normalizedLoops` */
      private def needsNormalization(tree: tpd.Tree)(implicit ctx: Context): Boolean = tree match {
        case _: tpd.Closure => true
        case _ => tree.symbol.is(Flags.Label)
      }
    }

    object Inlined extends InlinedExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Option[Term], List[Statement], Term)] = x match {
        case x: tpd.Inlined @unchecked =>
          Some((optional(x.call), x.bindings, x.expansion))
        case _ => None
      }
    }

    object Lambda extends LambdaExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[TypeTree])] = x match {
        case x: tpd.Closure @unchecked => Some((x.meth, optional(x.tpt)))
        case _ => None
      }
    }

    object If extends IfExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term, Term)] = x match {
        case x: tpd.If @unchecked => Some((x.cond, x.thenp, x.elsep))
        case _ => None
      }
    }

    object Match extends MatchExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef])] = x match {
        case x: tpd.Match @unchecked => Some((x.selector, x.cases))
        case _ => None
      }
    }

    object Try extends TryExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])] = x match {
        case x: tpd.Try @unchecked => Some((x.expr, x.cases, optional(x.finalizer)))
        case _ => None
      }
    }

    object Return extends ReturnExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[Term] = x match {
        case x: tpd.Return @unchecked => Some(x.expr)
        case _ => None
      }
    }

    object Repeated extends RepeatedExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[List[Term]] = x match {
        case x: tpd.SeqLiteral @unchecked => Some(x.elems)
        case _ => None
      }
    }

    object SelectOuter extends SelectOuterExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Int, Type)] = x match {
        case x: tpd.Select @unchecked =>
          x.name match {
            case NameKinds.OuterSelectName(_, levels) => Some((x.qualifier, levels, x.tpe.stripTypeVar))
            case _ => None
          }
        case _ => None
      }
    }

    object While extends WhileExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)] = x match {
        case Trees.Block((ddef: tpd.DefDef) :: Nil, expr) if expr.symbol.is(Flags.Label) && expr.symbol.name == nme.WHILE_PREFIX =>
          val Trees.If(cond, Trees.Block(bodyStats, _), _) = ddef.rhs
          Some((cond, loopBody(bodyStats)))
        case _ => None
      }
    }

    object DoWhile extends DoWhileExtractor {
      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)] = x match {
        case Trees.Block((ddef: tpd.DefDef) :: Nil, expr) if expr.symbol.is(Flags.Label) && expr.symbol.name == nme.DO_WHILE_PREFIX =>
          val Trees.Block(bodyStats, Trees.If(cond, _, _)) = ddef.rhs
          Some((loopBody(bodyStats), cond))
        case _ => None
      }
    }

    private def loopBody(stats: List[tpd.Tree])(implicit ctx: Context): tpd.Tree = stats match {
      case body :: Nil => body
      case stats => tpd.Block(stats.init, stats.last)
    }
  }

  // ----- CaseDef --------------------------------------------------

  type CaseDef = tpd.CaseDef

  def caseDefClassTag: ClassTag[CaseDef] = implicitly[ClassTag[CaseDef]]

  def CaseDefDeco(caseDef: CaseDef): CaseDefAPI = new CaseDefAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showCaseDef(caseDef)
    def pattern(implicit ctx: Context): Pattern = caseDef.pat
    def guard(implicit ctx: Context): Option[Term] = optional(caseDef.guard)
    def rhs(implicit ctx: Context): Term = caseDef.body
  }

  object CaseDef extends CaseDefExtractor {
    def unapply(x: CaseDef): Option[(Pattern, Option[Term], Term)] = x match {
      case x: tpd.CaseDef @unchecked =>
        Some(x.pat, optional(x.guard), x.body)
      case _ => None
    }
  }

  // ----- Patterns -------------------------------------------------

  type Pattern = tpd.Tree

  def PatternDeco(pattern: Pattern): PatternAPI = new PatternAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showPattern(pattern)
    def pos(implicit ctx: Context): Position = pattern.pos
    def tpe(implicit ctx: Context): Types.Type = pattern.tpe.stripTypeVar
  }

  def patternClassTag: ClassTag[Pattern] = implicitly[ClassTag[Pattern]]

  object Pattern extends PatternModule {

    object Value extends ValueExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[Term] = x match {
        case lit: tpd.Literal @unchecked => Some(lit)
        case ref: tpd.RefTree @unchecked if ref.isTerm => Some(ref)
        case ths: tpd.This @unchecked => Some(ths)
        case _ => None
      }
    }

    object Bind extends BindExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(String, Pattern)] = x match {
        case x: tpd.Bind @unchecked if x.name.isTermName => Some(x.name.toString, x.body)
        case _ => None
      }
    }

    object Unapply extends UnapplyExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])] = x match {
        case Trees.UnApply(fun, implicits, patterns) => Some((fun, implicits, effectivePatterns(patterns)))
        case Trees.Typed(Trees.UnApply(fun, implicits, patterns), _) => Some((fun, implicits, effectivePatterns(patterns)))
        case _ => None
      }
      private def effectivePatterns(patterns: List[Pattern]): List[Pattern] = patterns match {
        case patterns0 :+ Trees.SeqLiteral(elems, _) => patterns0 ::: elems
        case _ => patterns
      }
    }

    object Alternative extends AlternativeExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[List[Pattern]] = x match {
        case x: tpd.Alternative @unchecked => Some(x.trees)
        case _ => None
      }
    }

    object TypeTest extends TypeTestExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[TypeTree] = x match {
        case Trees.Typed(Trees.UnApply(_, _, _), _) => None
        case Trees.Typed(_, tpt) => Some(tpt)
        case _ => None
      }
    }

  }


  // ----- TypeOrBoundsTree ------------------------------------------------

  type TypeOrBoundsTree = tpd.Tree

  def TypeOrBoundsTreeDeco(tpt: TypeOrBoundsTree): TypeOrBoundsTreeAPI = new TypeOrBoundsTreeAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showTypeOrBoundsTree(tpt)
    def tpe(implicit ctx: Context): Type = tpt.tpe.stripTypeVar
  }

  // ----- TypeTrees ------------------------------------------------

  type TypeTree = tpd.Tree

  def TypeTreeDeco(tpt: TypeTree): TypeTreeAPI = new TypeTreeAPI {
    def pos(implicit ctx: Context): Position = tpt.pos
    def tpe(implicit ctx: Context): Types.Type = tpt.tpe.stripTypeVar
  }

  def typeTreeClassTag: ClassTag[TypeTree] = implicitly[ClassTag[TypeTree]]

  object TypeTree extends TypeTreeModule {

    def unapply(x: TypeTree)(implicit ctx: Context): Boolean = x.isType

    object Synthetic extends SyntheticExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Boolean = x match {
        case x @ Trees.TypeTree() => !x.tpe.isInstanceOf[Types.TypeBounds]
        case _ => false
      }
    }

    object TypeIdent extends TypeIdentExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[String] = x match {
        case x: tpd.Ident @unchecked if x.isType => Some(x.name.toString)
        case _ => None
      }
    }

    object TermSelect extends TermSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(Term, String)] = x match {
        case x: tpd.Select @unchecked if x.isType && x.qualifier.isTerm => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object TypeSelect extends TypeSelectExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, String)] = x match {
        case x: tpd.Select @unchecked if x.isType && x.qualifier.isType => Some(x.qualifier, x.name.toString)
        case _ => None
      }
    }

    object Singleton extends SingletonExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[Term] = x match {
        case x: tpd.SingletonTypeTree @unchecked => Some(x.ref)
        case _ => None
      }
    }

    object Refined extends RefinedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[Definition])] = x match {
        case x: tpd.RefinedTypeTree @unchecked => Some(x.tpt, x.refinements)
        case _ => None
      }
    }

    object Applied extends AppliedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, List[TypeOrBoundsTree])] = x match {
        case x: tpd.AppliedTypeTree @unchecked => Some(x.tpt, x.args)
        case _ => None
      }
    }

    object Annotated extends AnnotatedExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, Term)] = x match {
        case x: tpd.Annotated @unchecked => Some(x.arg, x.annot)
        case _ => None
      }
    }

    object And extends AndExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.AndTypeTree @unchecked => Some(x.left, x.right)
        case _ => None
      }
    }

    object Or extends OrExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
        case x: tpd.OrTypeTree @unchecked => Some(x.left, x.right)
        case _ => None
      }
    }

    object ByName extends ByNameExtractor {
      def unapply(x: TypeTree)(implicit ctx: Context): Option[TypeTree] = x match {
        case x: tpd.ByNameTypeTree @unchecked => Some(x.result)
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
        case x: tpd.Bind @unchecked if x.name.isTypeName => Some((x.name.toString, x.body))
        case _ => None
      }
    }
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  type TypeBoundsTree = tpd.Tree

  def TypeBoundsTreeDeco(bounds: TypeBoundsTree): TypeBoundsTreeAPI = new TypeBoundsTreeAPI {
    def tpe(implicit ctx: Context): TypeBounds = bounds.tpe.asInstanceOf[Types.TypeBounds]
    def low(implicit ctx: Context): TypeTree = bounds.asInstanceOf[tpd.TypeBoundsTree].lo
    def hi(implicit ctx: Context): TypeTree = bounds.asInstanceOf[tpd.TypeBoundsTree].hi
  }

  def typeBoundsTreeClassTag: ClassTag[TypeBoundsTree] = implicitly[ClassTag[TypeBoundsTree]]

  object TypeBoundsTree extends TypeBoundsTreeExtractor {
    def unapply(x: TypeBoundsTree)(implicit ctx: Context): Option[(TypeTree, TypeTree)] = x match {
      case x: tpd.TypeBoundsTree @unchecked => Some(x.lo, x.hi)
      case _ => None
    }
  }

  object SyntheticBounds extends SyntheticBoundsExtractor {
    def unapply(x: TypeBoundsTree)(implicit ctx: Context): Boolean = x match {
      case x @ Trees.TypeTree() => x.tpe.isInstanceOf[Types.TypeBounds]
      case Trees.Ident(nme.WILDCARD) => x.tpe.isInstanceOf[Types.TypeBounds]
      case _ => false
    }
  }

  // ===== Types ====================================================

  type TypeOrBounds = Types.Type

  def TypeOrBoundsDeco(tpe: Types.Type): TypeOrBoundsAPI = new TypeOrBoundsAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showTypeOrBounds(tpe)
  }

  // ----- Types ----------------------------------------------------

  type Type = Types.Type

  def TypeDeco(tpe: Type): TypeAPI = new TypeAPI {
    def =:=(other: Type)(implicit ctx: Context): Boolean = tpe =:= other
    def <:<(other: Type)(implicit ctx: Context): Boolean = tpe <:< other
  }

  type RecursiveType = Types.RecType
  type LambdaType[ParamInfo <: TypeOrBounds] = Types.LambdaType { type PInfo = ParamInfo }
  type MethodType = Types.MethodType
  type PolyType = Types.PolyType
  type TypeLambda = Types.TypeLambda

  def typeClassTag: ClassTag[Type] = implicitly[ClassTag[Type]]
  def recursiveTypeClassTag: ClassTag[RecursiveType] = implicitly[ClassTag[RecursiveType]]
  def methodTypeClassTag: ClassTag[MethodType] = implicitly[ClassTag[MethodType]]
  def polyTypeClassTag: ClassTag[PolyType] = implicitly[ClassTag[PolyType]]
  def typeLambdaClassTag: ClassTag[TypeLambda] = implicitly[ClassTag[TypeLambda]]

  def MethodTypeDeco(tpe: MethodType): MethodTypeAPI = new MethodTypeAPI {
    def isErased: Boolean = tpe.isErasedMethod
    def isImplicit: Boolean = tpe.isImplicitMethod
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[Type] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  def PolyTypeDeco(tpe: Types.PolyType): PolyTypeAPI = new PolyTypeAPI {
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[TypeBounds] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  def TypeLambdaDeco(tpe: Types.TypeLambda): TypeLambdaAPI = new TypeLambdaAPI {
    def paramNames(implicit ctx: Context): List[String] = tpe.paramNames.map(_.toString)
    def paramTypes(implicit ctx: Context): List[TypeBounds] = tpe.paramInfos
    def resultTpe(implicit ctx: Context): Type = tpe.resType
  }

  object Type extends TypeModule {

    def unapply(x: Type)(implicit ctx: Context): Boolean = x match {
      case x: Types.TypeBounds => false
      case x => x != Types.NoPrefix
    }

    object ConstantType extends ConstantTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Constant] = x match {
        case Types.ConstantType(value) => Some(value)
        case _ => None
      }
    }

    object SymRef extends SymRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Definition, TypeOrBounds /* Type | NoPrefix */)] = x  match {
        case tp: Types.NamedType =>
          tp.designator match {
            case sym: Symbol => Some((FromSymbol.definition(sym), tp.prefix))
            case _ => None
          }
        case _ => None
      }
    }

    object TermRef extends TermRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] = x match {
        case tp: Types.NamedType =>
          tp.designator match {
            case name: Names.TermName => Some(name.toString, tp.prefix)
            case _ => None
          }
        case _ => None
      }
    }

    object TypeRef extends TypeRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(String, TypeOrBounds /* Type | NoPrefix */)] = x match {
        case tp: Types.NamedType =>
          tp.designator match {
            case name: Names.TypeName => Some(name.toString, tp.prefix)
            case _ => None
          }
        case _ => None
      }
    }

    object SuperType extends SuperTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.SuperType(thistpe, supertpe) => Some(thistpe, supertpe)
        case _ => None
      }
    }

    object Refinement extends RefinementExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, String, TypeOrBounds /* Type | TypeBounds */)] = x match {
        case Types.RefinedType(parent, name, info) => Some(parent, name.toString, info)
        case _ => None
      }
    }

    object AppliedType extends AppliedTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, List[TypeOrBounds /* Type | TypeBounds */])] = x match {
        case Types.AppliedType(tycon, args) => Some((tycon.stripTypeVar, args.map(_.stripTypeVar)))
        case _ => None
      }
    }

    object AnnotatedType extends AnnotatedTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Term)] = x match {
        case Types.AnnotatedType(underlying, annot) => Some((underlying.stripTypeVar, annot.tree))
        case _ => None
      }
    }

    object TypeOf extends TypeOfExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Term)] = x match {
        case Types.TypeOf(underlying, tree) => Some((underlying.stripTypeVar, tree))
        case _ => None
      }
    }

    object AndType extends AndTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.AndType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object OrType extends OrTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(Type, Type)] = x match {
        case Types.OrType(left, right) => Some(left.stripTypeVar, right.stripTypeVar)
        case _ => None
      }
    }

    object ByNameType extends ByNameTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Type] = x match {
        case Types.ExprType(resType) => Some(resType.stripTypeVar)
        case _ => None
      }
    }

    object ParamRef extends ParamRefExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[(LambdaType[TypeOrBounds], Int)] = x match {
        case Types.TypeParamRef(binder, idx) =>
          Some((
            binder.asInstanceOf[LambdaType[TypeOrBounds]], // Cast to tpd
            idx))
        case Types.TermParamRef(binder, idx) => Some((binder, idx))
        case _ => None
      }
    }

    object ThisType extends ThisTypeExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[Type] = x match {
        case Types.ThisType(tp) => Some(tp)
        case _ => None
      }
    }

    object RecursiveThis extends RecursiveThisExtractor {
      def unapply(x: Type)(implicit ctx: Context): Option[RecursiveType] = x match {
        case Types.RecThis(binder) => Some(binder)
        case _ => None
      }
    }

    object RecursiveType extends RecursiveTypeExtractor {
      def unapply(x: RecursiveType)(implicit ctx: Context): Option[Type] = x match {
        case tp: Types.RecType => Some(tp.underlying.stripTypeVar)
        case _ => None
      }
    }

    object MethodType extends MethodTypeExtractor {
      def unapply(x: MethodType)(implicit ctx: Context): Option[(List[String], List[Type], Type)] = x match {
        case x: MethodType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object PolyType extends PolyTypeExtractor {
      def unapply(x: PolyType)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] = x match {
        case x: PolyType => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

    object TypeLambda extends TypeLambdaExtractor {
      def unapply(x: TypeLambda)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)] = x match {
        case x: TypeLambda => Some(x.paramNames.map(_.toString), x.paramInfos, x.resType)
        case _ => None
      }
    }

  }

  // ----- TypeBounds ------------------------------------------------

  type TypeBounds = Types.TypeBounds

  def typeBoundsClassTag: ClassTag[TypeBounds] = implicitly[ClassTag[TypeBounds]]

  object TypeBounds extends TypeBoundsExtractor {
    def unapply(x: TypeBounds)(implicit ctx: Context): Option[(Type, Type)] = x match {
      case x: Types.TypeBounds => Some(x.lo, x.hi)
      case _ => None
    }
  }

  def TypeBoundsDeco(tpe: TypeBounds): TypeBoundsAPI = new TypeBoundsAPI {
    def low(implicit ctx: Context): Type = tpe.lo
    def hi(implicit ctx: Context): Type = tpe.hi
  }

  // ----- NoPrefix --------------------------------------------------

  type NoPrefix = Types.NoPrefix.type

  def noPrefixClassTag: ClassTag[NoPrefix] = implicitly[ClassTag[NoPrefix]]

  object NoPrefix extends NoPrefixExtractor {
    def unapply(x: NoPrefix)(implicit ctx: Context): Boolean = x == Types.NoPrefix
  }

  // ===== Constants ================================================

  type Constant = Constants.Constant

  def ConstantDeco(const: Constant): ConstantAPI = new ConstantAPI {
    def show(implicit ctx: Context, s: Show[TastyImpl.this.type]): String = s.showConstant(const)
    def value: Any = const.value
  }

  def constantClassTag: ClassTag[Constant] = implicitly[ClassTag[Constant]]

  object Constant extends ConstantModule {

    object Unit extends UnitExtractor {
      def unapply(x: Constant): Boolean = x match {
        case x: Constants.Constant => x.tag == Constants.UnitTag
        case _ => false
      }
    }

    object Null extends NullExtractor {
      def unapply(x: Constant): Boolean =  x match {
        case x: Constants.Constant => x.tag == Constants.NullTag
        case _ => false
      }
    }

    object Boolean extends BooleanExtractor {
      def unapply(x: Constant): Option[Boolean] = x match {
        case x: Constants.Constant if x.tag == Constants.BooleanTag => Some(x.booleanValue)
        case _ => None
      }
    }

    object Byte extends ByteExtractor {
      def unapply(x: Constant): Option[Byte] = x match {
        case x: Constants.Constant if x.tag == Constants.ByteTag => Some(x.byteValue)
        case _ => None
      }
    }

    object Short extends ShortExtractor {
      def unapply(x: Constant): Option[Short] = x match {
        case x: Constants.Constant if x.tag == Constants.ShortTag => Some(x.shortValue)
        case _ => None
      }
    }

    object Char extends CharExtractor {
      def unapply(x: Constant): Option[Char] = x match {
        case x: Constants.Constant if x.tag == Constants.CharTag => Some(x.charValue)
        case _ => None
      }
    }

    object Int extends IntExtractor {
      def unapply(x: Constant): Option[Int] = x match {
        case x: Constants.Constant if x.tag == Constants.IntTag => Some(x.intValue)
        case _ => None
      }
    }

    object Long extends LongExtractor {
      def unapply(x: Constant): Option[Long] = x match {
        case x: Constants.Constant if x.tag == Constants.LongTag => Some(x.longValue)
        case _ => None
      }
    }

    object Float extends FloatExtractor {
      def unapply(x: Constant): Option[Float] = x match {
        case x: Constants.Constant if x.tag == Constants.FloatTag => Some(x.floatValue)
        case _ => None
      }
    }

    object Double extends DoubleExtractor {
      def unapply(x: Constant): Option[Double] = x match {
        case x: Constants.Constant if x.tag == Constants.DoubleTag => Some(x.doubleValue)
        case _ => None
      }
    }

    object String extends StringExtractor {
      def unapply(x: Constant): Option[String] = x match {
        case x: Constants.Constant if x.tag == Constants.StringTag => Some(x.stringValue)
        case _ => None
      }
    }

    object ClassTag extends ClassTagExtractor {
      def unapply(x: Constant): Option[Type] = x match {
        case x: Constants.Constant if x.tag == Constants.ClazzTag => Some(x.typeValue)
        case _ => None
      }
    }

    object Symbol extends SymbolExtractor {
      def unapply(x: Constant): Option[scala.Symbol] = x match {
        case x: Constants.Constant if x.tag == Constants.ScalaSymbolTag => Some(x.scalaSymbolValue)
        case _ => None
      }
    }
  }

  // ===== Signature ================================================

  type Signature = core.Signature

  def signatureClassTag: ClassTag[Signature] = implicitly[ClassTag[Signature]]

  object Signature extends SignatureExtractor {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)] = {
      Some((x.paramsSig.map(_.toString), x.resSig.toString))
    }
  }

  def SignatureDeco(sig: Signature): SignatureAPI = new SignatureAPI {
    def paramSigs: List[String] = sig.paramsSig.map(_.toString)
    def resultSig: String = sig.resSig.toString
  }

  // ===== Positions ================================================

  type Position = SourcePosition

  def PositionDeco(pos: Position): PositionAPI = new PositionAPI {
    def start = pos.start
    def end = pos.end

    def sourceFile = pos.source.file.jpath

    def startLine = pos.startLine
    def endLine = pos.endLine

    def startColumn = pos.startColumn
    def endColumn = pos.endColumn
  }

  // ===== Helpers ==================================================

  private def optional[T <: Trees.Tree[_]](tree: T): Option[tree.type] =
    if (tree.isEmpty) None else Some(tree)

}
