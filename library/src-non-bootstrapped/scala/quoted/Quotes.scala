package scala.quoted

import scala.reflect.TypeTest

inline def quotes(using q: Quotes): q.type = q

trait Quotes { self: runtime.QuoteUnpickler & runtime.QuoteMatching =>

  extension [T](self: Expr[T])
    def show: String

    def matches(that: Expr[Any]): Boolean

    def value(using FromExpr[T]): Option[T] =
      given Quotes = Quotes.this
      summon[FromExpr[T]].unapply(self)

    def valueOrError(using FromExpr[T]): T =
      val fromExpr = summon[FromExpr[T]]
      def reportError =
        val msg = s"Expected a known value. \n\nThe value of: ${self.show}\ncould not be extracted using $fromExpr"
        reflect.report.throwError(msg, self)
      given Quotes = Quotes.this
      fromExpr.unapply(self).getOrElse(reportError)

  end extension

  extension [X](self: Expr[Any])
    def isExprOf(using Type[X]): Boolean

    def asExprOf(using Type[X]): Expr[X]
  end extension

  val reflect: reflectModule

  trait reflectModule { self: reflect.type =>

    extension (expr: Expr[Any])
      def asTerm: Term

    type Tree <: AnyRef

    val Tree: TreeModule

    trait TreeModule { this: Tree.type =>
    }

    given TreeMethods: TreeMethods

    trait TreeMethods {

      extension (self: Tree)
        def pos: Position

        def symbol: Symbol

        def show(using Printer[Tree]): String

        def isExpr: Boolean

        def asExpr: Expr[Any]
      end extension

      extension [T](self: Tree)
        def asExprOf(using Type[T]): Expr[T]

      extension [ThisTree <: Tree](self: ThisTree)
        def changeOwner(newOwner: Symbol): ThisTree
      end extension

    }

    type PackageClause <: Tree

    given PackageClauseTypeTest: TypeTest[Tree, PackageClause]

    val PackageClause: PackageClauseModule

    trait PackageClauseModule { this: PackageClause.type =>
      def apply(pid: Ref, stats: List[Tree]): PackageClause
      def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause
      def unapply(tree: PackageClause): (Ref, List[Tree])
    }

    given PackageClauseMethods: PackageClauseMethods

    trait PackageClauseMethods:
      extension (self: PackageClause)
        def pid: Ref
        def stats: List[Tree]
      end extension
    end PackageClauseMethods

    type Import <: Statement

    given ImportTypeTest: TypeTest[Tree, Import]

    val Import: ImportModule

    trait ImportModule { this: Import.type =>
      def apply(expr: Term, selectors: List[Selector]): Import
      def copy(original: Tree)(expr: Term, selectors: List[Selector]): Import
      def unapply(tree: Import): (Term, List[Selector])
    }

    given ImportMethods: ImportMethods

    trait ImportMethods:
      extension (self: Import)
        def expr: Term
        def selectors: List[Selector]
      end extension
    end ImportMethods

    type Export <: Statement

    given ExportTypeTest: TypeTest[Tree, Export]

    val Export: ExportModule

    trait ExportModule { this: Export.type =>
      def unapply(tree: Export): (Term, List[Selector])
    }

    given ExportMethods: ExportMethods

    trait ExportMethods:
      extension (self: Export)
        def expr: Term
        def selectors: List[Selector]
      end extension
    end ExportMethods

    type Statement <: Tree

    given StatementTypeTest: TypeTest[Tree, Statement]

    type Definition <: Statement

    given DefinitionTypeTest: TypeTest[Tree, Definition]

    val Definition: DefinitionModule

    trait DefinitionModule { this: Definition.type => }

    given DefinitionMethods: DefinitionMethods

    trait DefinitionMethods:
      extension (self: Definition)
        def name: String
      end extension
    end DefinitionMethods

    type ClassDef <: Definition

    given ClassDefTypeTest: TypeTest[Tree, ClassDef]

    val ClassDef: ClassDefModule

    trait ClassDefModule { this: ClassDef.type =>
      def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def unapply(cdef: ClassDef): (String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])
    }

    given ClassDefMethods: ClassDefMethods

    trait ClassDefMethods:
      extension (self: ClassDef)
        def constructor: DefDef
        def parents: List[Tree /* Term | TypeTree */]
        def derived: List[TypeTree]
        def self: Option[ValDef]
        def body: List[Statement]
      end extension
    end ClassDefMethods

    type DefDef <: Definition

    given DefDefTypeTest: TypeTest[Tree, DefDef]

    val DefDef: DefDefModule

    trait DefDefModule { this: DefDef.type =>
      def apply(symbol: Symbol, rhsFn: List[TypeRepr] => List[List[Term]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): (String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])
    }

    given DefDefMethods: DefDefMethods

    trait DefDefMethods:
      extension (self: DefDef)
        def typeParams: List[TypeDef]
        def paramss: List[List[ValDef]]
        def returnTpt: TypeTree
        def rhs: Option[Term]
      end extension
    end DefDefMethods

    type ValDef <: Definition

    given ValDefTypeTest: TypeTest[Tree, ValDef]

    val ValDef: ValDefModule

    trait ValDefModule { this: ValDef.type =>
      def apply(symbol: Symbol, rhs: Option[Term]): ValDef
      def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef
      def unapply(vdef: ValDef): (String, TypeTree, Option[Term])

      def let(owner: Symbol, name: String, rhs: Term)(body: Ident => Term): Term

      def let(owner: Symbol, rhs: Term)(body: Ident => Term): Term =
        let(owner, "x", rhs)(body)

      def let(owner: Symbol, terms: List[Term])(body: List[Ident] => Term): Term
    }

    given ValDefMethods: ValDefMethods

    trait ValDefMethods:
      extension (self: ValDef)
        def tpt: TypeTree
        def rhs: Option[Term]
      end extension
    end ValDefMethods

    type TypeDef <: Definition

    given TypeDefTypeTest: TypeTest[Tree, TypeDef]

    val TypeDef: TypeDefModule

    trait TypeDefModule { this: TypeDef.type =>
      def apply(symbol: Symbol): TypeDef
      def copy(original: Tree)(name: String, rhs: Tree /*TypeTree | TypeBoundsTree*/): TypeDef
      def unapply(tdef: TypeDef): (String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)
    }

    given TypeDefMethods: TypeDefMethods

    trait TypeDefMethods:
      extension (self: TypeDef)
        def rhs: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeDefMethods

    type Term <: Statement

    given TermTypeTest: TypeTest[Tree, Term]

    val Term: TermModule

    trait TermModule { this: Term.type =>

      def betaReduce(term: Term): Option[Term]

    }

    given TermMethods: TermMethods

    trait TermMethods {
      extension (self: Term)

        def tpe: TypeRepr

        def underlyingArgument: Term

        def underlying: Term

        def etaExpand(owner: Symbol): Term

        def appliedTo(arg: Term): Term

        def appliedTo(arg: Term, args: Term*): Term

        def appliedToArgs(args: List[Term]): Apply

        def appliedToArgss(argss: List[List[Term]]): Term

        def appliedToNone: Apply

        def appliedToType(targ: TypeRepr): Term

        def appliedToTypes(targs: List[TypeRepr]): Term

        def appliedToTypeTrees(targs: List[TypeTree]): Term

        def select(sym: Symbol): Select

      end extension

    }

    type Ref <: Term

    given RefTypeTest: TypeTest[Tree, Ref]

    val Ref: RefModule

    trait RefModule { this: Ref.type =>

      def term(tp: TermRef): Ref

      def apply(sym: Symbol): Ref
    }

    type Ident <: Ref

    given IdentTypeTest: TypeTest[Tree, Ident]

    val Ident: IdentModule

    trait IdentModule { this: Ident.type =>
      def apply(tmref: TermRef): Term

      def copy(original: Tree)(name: String): Ident

      def unapply(tree: Ident): Some[String]
    }

    given IdentMethods: IdentMethods

    trait IdentMethods:
      extension (self: Ident)
        def name: String
      end extension
    end IdentMethods

    type Select <: Ref

    given SelectTypeTest: TypeTest[Tree, Select]

    val Select: SelectModule

    trait SelectModule { this: Select.type =>
      def apply(qualifier: Term, symbol: Symbol): Select

      def unique(qualifier: Term, name: String): Select

      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Apply

      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Apply

      def copy(original: Tree)(qualifier: Term, name: String): Select

      def unapply(x: Select): (Term, String)
    }

    given SelectMethods: SelectMethods

    trait SelectMethods:
      extension (self: Select)
        def qualifier: Term
        def name: String
        def signature: Option[Signature]
      end extension
    end SelectMethods

    given LiteralTypeTest: TypeTest[Tree, Literal]

    type Literal <: Term

    val Literal: LiteralModule

    trait LiteralModule { this: Literal.type =>

      def apply(constant: Constant): Literal

      def copy(original: Tree)(constant: Constant): Literal

      def unapply(x: Literal): Some[Constant]
    }

    given LiteralMethods: LiteralMethods

    trait LiteralMethods:
      extension (self: Literal)
        def constant: Constant
      end extension
    end LiteralMethods

    type This <: Term

    given ThisTypeTest: TypeTest[Tree, This]

    val This: ThisModule

    trait ThisModule { this: This.type =>

      def apply(cls: Symbol): This

      def copy(original: Tree)(qual: Option[String]): This

      def unapply(x: This): Some[Option[String]]
    }

    given ThisMethods: ThisMethods

    trait ThisMethods:
      extension (self: This)
        def id: Option[String]
      end extension
    end ThisMethods

    type New <: Term

    given NewTypeTest: TypeTest[Tree, New]

    val New: NewModule

    trait NewModule { this: New.type =>

      def apply(tpt: TypeTree): New

      def copy(original: Tree)(tpt: TypeTree): New

      def unapply(x: New): Some[TypeTree]
    }

    given NewMethods: NewMethods

    trait NewMethods:
      extension (self: New)
        def tpt: TypeTree
      end extension
    end NewMethods

    type NamedArg <: Term

    given NamedArgTypeTest: TypeTest[Tree, NamedArg]

    val NamedArg: NamedArgModule

    trait NamedArgModule { this: NamedArg.type =>

      def apply(name: String, arg: Term): NamedArg

      def copy(original: Tree)(name: String, arg: Term): NamedArg

      def unapply(x: NamedArg): (String, Term)
    }

    given NamedArgMethods: NamedArgMethods

    trait NamedArgMethods:
      extension (self: NamedArg)
        def name: String
        def value: Term
      end extension
    end NamedArgMethods

    type Apply <: Term

    given ApplyTypeTest: TypeTest[Tree, Apply]

    val Apply: ApplyModule

    trait ApplyModule { this: Apply.type =>

      def apply(fun: Term, args: List[Term]): Apply

      def copy(original: Tree)(fun: Term, args: List[Term]): Apply

      def unapply(x: Apply): (Term, List[Term])
    }

    given ApplyMethods: ApplyMethods

    trait ApplyMethods:
      extension (self: Apply)
        def fun: Term
        def args: List[Term]
      end extension
    end ApplyMethods

    type TypeApply <: Term

    given TypeApplyTypeTest: TypeTest[Tree, TypeApply]

    val TypeApply: TypeApplyModule

    trait TypeApplyModule { this: TypeApply.type =>

      def apply(fun: Term, args: List[TypeTree]): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

      def unapply(x: TypeApply): (Term, List[TypeTree])
    }

    given TypeApplyMethods: TypeApplyMethods

    trait TypeApplyMethods:
      extension (self: TypeApply)
        def fun: Term
        def args: List[TypeTree]
      end extension
    end TypeApplyMethods

    given SuperTypeTest: TypeTest[Tree, Super]

    type Super <: Term

    val Super: SuperModule

    trait SuperModule { this: Super.type =>

      def apply(qual: Term, mix: Option[String]): Super

      def copy(original: Tree)(qual: Term, mix: Option[String]): Super

      def unapply(x: Super): (Term, Option[String])
    }

    given SuperMethods: SuperMethods

    trait SuperMethods:
      extension (self: Super)
        def qualifier: Term
        def id: Option[String]
        def idPos: Position
      end extension
    end SuperMethods

    given TypedTypeTest: TypeTest[Tree, Typed]

    type Typed <: Term

    val Typed: TypedModule

    trait TypedModule { this: Typed.type =>

      def apply(expr: Term, tpt: TypeTree): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed

      def unapply(x: Typed): (Term, TypeTree)
    }

    given TypedMethods: TypedMethods

    trait TypedMethods:
      extension (self: Typed)
        def expr: Term
        def tpt: TypeTree
      end extension
    end TypedMethods

    type Assign <: Term

    given AssignTypeTest: TypeTest[Tree, Assign]

    val Assign: AssignModule

    trait AssignModule { this: Assign.type =>

      def apply(lhs: Term, rhs: Term): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term): Assign

      def unapply(x: Assign): (Term, Term)
    }

    given AssignMethods: AssignMethods

    trait AssignMethods:
      extension (self: Assign)
        def lhs: Term
        def rhs: Term
      end extension
    end AssignMethods

    type Block <: Term

    given BlockTypeTest: TypeTest[Tree, Block]

    val Block: BlockModule

    trait BlockModule { this: Block.type =>

      def apply(stats: List[Statement], expr: Term): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term): Block

      def unapply(x: Block): (List[Statement], Term)
    }

    given BlockMethods: BlockMethods

    trait BlockMethods:
      extension (self: Block)
        def statements: List[Statement]
        def expr: Term
      end extension
    end BlockMethods

    given ClosureTypeTest: TypeTest[Tree, Closure]

    type Closure <: Term

    val Closure: ClosureModule

    trait ClosureModule { this: Closure.type =>

      def apply(meth: Term, tpe: Option[TypeRepr]): Closure

      def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure

      def unapply(x: Closure): (Term, Option[TypeRepr])
    }

    given ClosureMethods: ClosureMethods

    trait ClosureMethods:
      extension (self: Closure)
        def meth: Term
        def tpeOpt: Option[TypeRepr]
      end extension
    end ClosureMethods

    val Lambda: LambdaModule

    trait LambdaModule { this: Lambda.type =>
      def unapply(tree: Block): Option[(List[ValDef], Term)]

      def apply(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block
    }

    given IfTypeTest: TypeTest[Tree, If]

    type If <: Term

    val If: IfModule

    trait IfModule { this: If.type =>

      def apply(cond: Term, thenp: Term, elsep: Term): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If

      def unapply(tree: If): (Term, Term, Term)
    }

    given IfMethods: IfMethods

    trait IfMethods:
      extension (self: If)
        def cond: Term
        def thenp: Term
        def elsep: Term
        def isInline: Boolean
      end extension
    end IfMethods

    type Match <: Term

    given MatchTypeTest: TypeTest[Tree, Match]

    val Match: MatchModule

    trait MatchModule { this: Match.type =>

      def apply(selector: Term, cases: List[CaseDef]): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match

      def unapply(x: Match): (Term, List[CaseDef])
    }

    given MatchMethods: MatchMethods

    trait MatchMethods:
      extension (self: Match)
        def scrutinee: Term
        def cases: List[CaseDef]
        def isInline: Boolean
      end extension
    end MatchMethods

    type SummonFrom <: Term

    given SummonFromTypeTest: TypeTest[Tree, SummonFrom]

    val SummonFrom: SummonFromModule

    trait SummonFromModule { this: SummonFrom.type =>

      def apply(cases: List[CaseDef]): SummonFrom

      def copy(original: Tree)(cases: List[CaseDef]): SummonFrom

      def unapply(x: SummonFrom): Some[List[CaseDef]]
    }

    given SummonFromMethods: SummonFromMethods

    trait SummonFromMethods:
      extension (self: SummonFrom)
        def cases: List[CaseDef]
      end extension
    end SummonFromMethods

    type Try <: Term

    given TryTypeTest: TypeTest[Tree, Try]

    val Try: TryModule

    trait TryModule { this: Try.type =>

      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      def unapply(x: Try): (Term, List[CaseDef], Option[Term])
    }

    given TryMethods: TryMethods

    trait TryMethods:
      extension (self: Try)
        def body: Term
        def cases: List[CaseDef]
        def finalizer: Option[Term]
      end extension
    end TryMethods

    given ReturnTypeTest: TypeTest[Tree, Return]

    type Return <: Term

    val Return: ReturnModule

    trait ReturnModule { this: Return.type =>

      def apply(expr: Term, from: Symbol): Return

      def copy(original: Tree)(expr: Term, from: Symbol): Return

      def unapply(x: Return): (Term, Symbol)
    }

    given ReturnMethods: ReturnMethods

    trait ReturnMethods:
      extension (self: Return)
        def expr: Term
        def from: Symbol
      end extension
    end ReturnMethods

    type Repeated <: Term

    given RepeatedTypeTest: TypeTest[Tree, Repeated]

    val Repeated: RepeatedModule

    trait RepeatedModule { this: Repeated.type =>
      def apply(elems: List[Term], tpt: TypeTree): Repeated
      def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated
      def unapply(x: Repeated): (List[Term], TypeTree)
    }

    given RepeatedMethods: RepeatedMethods

    trait RepeatedMethods:
      extension (self: Repeated)
        def elems: List[Term]
        def elemtpt: TypeTree
      end extension
    end RepeatedMethods

    type Inlined <: Term

    given InlinedTypeTest: TypeTest[Tree, Inlined]

    val Inlined: InlinedModule

    trait InlinedModule { this: Inlined.type =>
      def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def unapply(x: Inlined): (Option[Tree /* Term | TypeTree */], List[Definition], Term)
    }

    given InlinedMethods: InlinedMethods

    trait InlinedMethods:
      extension (self: Inlined)
        def call: Option[Tree /* Term | TypeTree */]
        def bindings: List[Definition]
        def body: Term
      end extension
    end InlinedMethods

    type SelectOuter <: Term

    given SelectOuterTypeTest: TypeTest[Tree, SelectOuter]

    val SelectOuter: SelectOuterModule

    trait SelectOuterModule { this: SelectOuter.type =>
      def apply(qualifier: Term, name: String, levels: Int): SelectOuter
      def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter
      def unapply(x: SelectOuter): (Term, String, Int)
    }

    given SelectOuterMethods: SelectOuterMethods

    trait SelectOuterMethods:
      extension (self: SelectOuter)
        def qualifier: Term
        def name: String
        def level: Int
      end extension
    end SelectOuterMethods

    type While <: Term

    given WhileTypeTest: TypeTest[Tree, While]

    val While: WhileModule

    trait WhileModule { this: While.type =>

      def apply(cond: Term, body: Term): While

      def copy(original: Tree)(cond: Term, body: Term): While

      def unapply(x: While): (Term, Term)
    }

    given WhileMethods: WhileMethods

    trait WhileMethods:
      extension (self: While)
        def cond: Term
        def body: Term
      end extension
    end WhileMethods

    type TypeTree <: Tree

    given TypeTreeTypeTest: TypeTest[Tree, TypeTree]

    val TypeTree: TypeTreeModule

    trait TypeTreeModule { this: TypeTree.type =>
      def of[T <: AnyKind](using Type[T]): TypeTree
    }

    given TypeTreeMethods: TypeTreeMethods

    trait TypeTreeMethods:
      extension (self: TypeTree)
        def tpe: TypeRepr
      end extension
    end TypeTreeMethods

    type Inferred <: TypeTree

    given InferredTypeTest: TypeTest[Tree, Inferred]

    val Inferred: InferredModule

    trait InferredModule { this: Inferred.type =>
      def apply(tpe: TypeRepr): Inferred
      def unapply(x: Inferred): true
    }

    type TypeIdent <: TypeTree

    given TypeIdentTypeTest: TypeTest[Tree, TypeIdent]

    val TypeIdent: TypeIdentModule

    trait TypeIdentModule { this: TypeIdent.type =>
      def apply(sym: Symbol): TypeTree
      def copy(original: Tree)(name: String): TypeIdent
      def unapply(x: TypeIdent): Some[String]
    }

    given TypeIdentMethods: TypeIdentMethods

    trait TypeIdentMethods:
      extension (self: TypeIdent)
        def name: String
      end extension
    end TypeIdentMethods

    type TypeSelect <: TypeTree

    given TypeSelectTypeTest: TypeTest[Tree, TypeSelect]

    val TypeSelect: TypeSelectModule

    trait TypeSelectModule { this: TypeSelect.type =>
      def apply(qualifier: Term, name: String): TypeSelect
      def copy(original: Tree)(qualifier: Term, name: String): TypeSelect
      def unapply(x: TypeSelect): (Term, String)
    }

    given TypeSelectMethods: TypeSelectMethods

    trait TypeSelectMethods:
      extension (self: TypeSelect)
        def qualifier: Term
        def name: String
      end extension
    end TypeSelectMethods

    type TypeProjection <: TypeTree

    given TypeProjectionTypeTest: TypeTest[Tree, TypeProjection]

    val TypeProjection: TypeProjectionModule

    trait TypeProjectionModule { this: TypeProjection.type =>
      def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection
      def unapply(x: TypeProjection): (TypeTree, String)
    }

    given TypeProjectionMethods: TypeProjectionMethods

    trait TypeProjectionMethods:
      extension (self: TypeProjection)
        def qualifier: TypeTree
        def name: String
      end extension
    end TypeProjectionMethods

    type Singleton <: TypeTree

    given SingletonTypeTest: TypeTest[Tree, Singleton]

    val Singleton: SingletonModule

    trait SingletonModule { this: Singleton.type =>
      def apply(ref: Term): Singleton
      def copy(original: Tree)(ref: Term): Singleton
      def unapply(x: Singleton): Some[Term]
    }

    given SingletonMethods: SingletonMethods

    trait SingletonMethods:
      extension (self: Singleton)
        def ref: Term
      end extension
    end SingletonMethods

    type Refined <: TypeTree

    given RefinedTypeTest: TypeTest[Tree, Refined]

    val Refined: RefinedModule

    trait RefinedModule { this: Refined.type =>
      def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined
      def unapply(x: Refined): (TypeTree, List[Definition])
    }

    given RefinedMethods: RefinedMethods

    trait RefinedMethods:
      extension (self: Refined)
        def tpt: TypeTree
        def refinements: List[Definition]
      end extension
    end RefinedMethods

    type Applied <: TypeTree

    given AppliedTypeTest: TypeTest[Tree, Applied]

    val Applied: AppliedModule

    trait AppliedModule { this: Applied.type =>
      def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def unapply(x: Applied): (TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])
    }

    given AppliedMethods: AppliedMethods

    trait AppliedMethods:
      extension (self: Applied)
        def tpt: TypeTree
        def args: List[Tree /*TypeTree | TypeBoundsTree*/]
      end extension
    end AppliedMethods

    type Annotated <: TypeTree

    given AnnotatedTypeTest: TypeTest[Tree, Annotated]

    val Annotated: AnnotatedModule

    trait AnnotatedModule { this: Annotated.type =>
      def apply(arg: TypeTree, annotation: Term): Annotated
      def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated
      def unapply(x: Annotated): (TypeTree, Term)
    }

    given AnnotatedMethods: AnnotatedMethods

    trait AnnotatedMethods:
      extension (self: Annotated)
        def arg: TypeTree
        def annotation: Term
      end extension
    end AnnotatedMethods

    type MatchTypeTree <: TypeTree

    given MatchTypeTreeTypeTest: TypeTest[Tree, MatchTypeTree]

    val MatchTypeTree: MatchTypeTreeModule

    trait MatchTypeTreeModule { this: MatchTypeTree.type =>
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def unapply(x: MatchTypeTree): (Option[TypeTree], TypeTree, List[TypeCaseDef])
    }

    given MatchTypeTreeMethods: MatchTypeTreeMethods

    trait MatchTypeTreeMethods:
      extension (self: MatchTypeTree)
        def bound: Option[TypeTree]
        def selector: TypeTree
        def cases: List[TypeCaseDef]
      end extension
    end MatchTypeTreeMethods

    type ByName <: TypeTree

    given ByNameTypeTest: TypeTest[Tree, ByName]

    val ByName: ByNameModule

    trait ByNameModule { this: ByName.type =>
      def apply(result: TypeTree): ByName
      def copy(original: Tree)(result: TypeTree): ByName
      def unapply(x: ByName): Some[TypeTree]
    }

    given ByNameMethods: ByNameMethods

    trait ByNameMethods:
      extension (self: ByName)
        def result: TypeTree
      end extension
    end ByNameMethods

    type LambdaTypeTree <: TypeTree

    given LambdaTypeTreeTypeTest: TypeTest[Tree, LambdaTypeTree]

    val LambdaTypeTree: LambdaTypeTreeModule

    trait LambdaTypeTreeModule { this: LambdaTypeTree.type =>
      def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def unapply(tree: LambdaTypeTree): (List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)
    }

    given LambdaTypeTreeMethods: LambdaTypeTreeMethods

    trait LambdaTypeTreeMethods:
      extension (self: LambdaTypeTree)
        def tparams: List[TypeDef]
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end LambdaTypeTreeMethods

    type TypeBind <: TypeTree

    given TypeBindTypeTest: TypeTest[Tree, TypeBind]

    val TypeBind: TypeBindModule

    trait TypeBindModule { this: TypeBind.type =>
      def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind
      def unapply(x: TypeBind): (String, Tree /*TypeTree | TypeBoundsTree*/)
    }

    given TypeBindMethods: TypeBindMethods

    trait TypeBindMethods:
      extension (self: TypeBind)
        def name: String
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeBindMethods

    type TypeBlock <: TypeTree

    given TypeBlockTypeTest: TypeTest[Tree, TypeBlock]

    val TypeBlock: TypeBlockModule

    trait TypeBlockModule { this: TypeBlock.type =>
      def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def unapply(x: TypeBlock): (List[TypeDef], TypeTree)
    }

    given TypeBlockMethods: TypeBlockMethods

    trait TypeBlockMethods:
      extension (self: TypeBlock)
        def aliases: List[TypeDef]
        def tpt: TypeTree
      end extension
    end TypeBlockMethods

    type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

    given TypeBoundsTreeTypeTest: TypeTest[Tree, TypeBoundsTree]

    val TypeBoundsTree: TypeBoundsTreeModule

    trait TypeBoundsTreeModule { this: TypeBoundsTree.type =>
      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def unapply(x: TypeBoundsTree): (TypeTree, TypeTree)
    }

    given TypeBoundsTreeMethods: TypeBoundsTreeMethods

    trait TypeBoundsTreeMethods:
      extension (self: TypeBoundsTree)
        def tpe: TypeBounds
        def low: TypeTree
        def hi: TypeTree
      end extension
    end TypeBoundsTreeMethods

    type WildcardTypeTree  <: Tree

    given WildcardTypeTreeTypeTest: TypeTest[Tree, WildcardTypeTree]

    val WildcardTypeTree: WildcardTypeTreeModule

    trait WildcardTypeTreeModule { this: WildcardTypeTree.type =>
      def apply(tpe: TypeRepr): WildcardTypeTree
      def unapply(x: WildcardTypeTree): true
    }

    given WildcardTypeTreeMethods: WildcardTypeTreeMethods

    trait WildcardTypeTreeMethods:
      extension (self: WildcardTypeTree)
        def tpe: TypeRepr
      end extension
    end WildcardTypeTreeMethods

    type CaseDef <: Tree

    given CaseDefTypeTest: TypeTest[Tree, CaseDef]

    val CaseDef: CaseDefModule

    trait CaseDefModule { this: CaseDef.type =>
      def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def unapply(x: CaseDef): (Tree, Option[Term], Term)
    }

    given CaseDefMethods: CaseDefMethods

    trait CaseDefMethods:
      extension (self: CaseDef)
        def pattern: Tree
        def guard: Option[Term]
        def rhs: Term
      end extension
    end CaseDefMethods

    type TypeCaseDef <: Tree

    given TypeCaseDefTypeTest: TypeTest[Tree, TypeCaseDef]

    val TypeCaseDef: TypeCaseDefModule

    trait TypeCaseDefModule { this: TypeCaseDef.type =>
      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def unapply(tree: TypeCaseDef): (TypeTree, TypeTree)
    }

    given TypeCaseDefMethods: TypeCaseDefMethods

    trait TypeCaseDefMethods:
      extension (self: TypeCaseDef)
        def pattern: TypeTree
        def rhs: TypeTree
      end extension
    end TypeCaseDefMethods

    type Bind <: Tree

    given BindTypeTest: TypeTest[Tree, Bind]

    val Bind: BindModule

    trait BindModule { this: Bind.type =>
      def apply(sym: Symbol, pattern: Tree): Bind
      def copy(original: Tree)(name: String, pattern: Tree): Bind
      def unapply(pattern: Bind): (String, Tree)
    }

    given BindMethods: BindMethods

    trait BindMethods:
      extension (self: Bind)
        def name: String
        def pattern: Tree
      end extension
    end BindMethods

    type Unapply <: Tree

    given UnapplyTypeTest: TypeTest[Tree, Unapply]

    val Unapply: UnapplyModule

    trait UnapplyModule { this: Unapply.type =>
      def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      def unapply(x: Unapply): (Term, List[Term], List[Tree])
    }

    given UnapplyMethods: UnapplyMethods

    trait UnapplyMethods:
      extension (self: Unapply)
        def fun: Term
        def implicits: List[Term]
        def patterns: List[Tree]
      end extension
    end UnapplyMethods

    type Alternatives <: Tree

    given AlternativesTypeTest: TypeTest[Tree, Alternatives]

    val Alternatives: AlternativesModule

    trait AlternativesModule { this: Alternatives.type =>
      def apply(patterns: List[Tree]): Alternatives
      def copy(original: Tree)(patterns: List[Tree]): Alternatives
      def unapply(x: Alternatives): Some[List[Tree]]
    }

    given AlternativesMethods: AlternativesMethods

    trait AlternativesMethods:
      extension (self: Alternatives)
        def patterns: List[Tree]
      end extension
    end AlternativesMethods

    type Selector <: AnyRef

    val Selector: SelectorModule

    trait SelectorModule { this: Selector.type => }

    type SimpleSelector <: Selector

    given SimpleSelectorTypeTest: TypeTest[Selector, SimpleSelector]

    val SimpleSelector: SimpleSelectorModule

    trait SimpleSelectorModule { this: SimpleSelector.type =>
      def unapply(x: SimpleSelector): Some[String]
    }

    given SimpleSelectorMethods: SimpleSelectorMethods

    trait SimpleSelectorMethods:
      extension (self: SimpleSelector)
        def name: String
        def namePos: Position
      end extension
    end SimpleSelectorMethods

    type RenameSelector <: Selector

    given RenameSelectorTypeTest: TypeTest[Selector, RenameSelector]

    val RenameSelector: RenameSelectorModule

    trait RenameSelectorModule { this: RenameSelector.type =>
      def unapply(x: RenameSelector): (String, String)
    }

    given RenameSelectorMethods: RenameSelectorMethods

    trait RenameSelectorMethods:
      extension (self: RenameSelector)
        def fromName: String
        def fromPos: Position
        def toName: String
        def toPos: Position
      end extension
    end RenameSelectorMethods

    type OmitSelector <: Selector

    given OmitSelectorTypeTest: TypeTest[Selector, OmitSelector]

    val OmitSelector: OmitSelectorModule

    trait OmitSelectorModule { this: OmitSelector.type =>
      def unapply(x: OmitSelector): Some[String]
    }

    given OmitSelectorMethods: OmitSelectorMethods

    trait OmitSelectorMethods:
      extension (self: OmitSelector)
        def name: String
        def namePos: Position
    end OmitSelectorMethods

    type GivenSelector <: Selector

    given GivenSelectorTypeTest: TypeTest[Selector, GivenSelector]

    val GivenSelector: GivenSelectorModule

    trait GivenSelectorModule { this: GivenSelector.type =>
      def unapply(x: GivenSelector): Some[Option[TypeTree]]
    }

    given GivenSelectorMethods: GivenSelectorMethods

    trait GivenSelectorMethods:
      extension (self: GivenSelector)
        def bound: Option[TypeTree]
    end GivenSelectorMethods

    type TypeRepr

    val TypeRepr: TypeReprModule

    trait TypeReprModule { this: TypeRepr.type =>
      def of[T <: AnyKind](using Type[T]): TypeRepr

      def typeConstructorOf(clazz: Class[?]): TypeRepr
    }

    given TypeReprMethods: TypeReprMethods

    trait TypeReprMethods {
      extension (self: TypeRepr)

        def show(using Printer[TypeRepr]): String

        def asType: Type[?]

        def =:=(that: TypeRepr): Boolean

        def <:<(that: TypeRepr): Boolean

        def widen: TypeRepr

        def widenTermRefExpr: TypeRepr

        def dealias: TypeRepr

        def simplified: TypeRepr

        def classSymbol: Option[Symbol]
        def typeSymbol: Symbol
        def termSymbol: Symbol
        def isSingleton: Boolean
        def memberType(member: Symbol): TypeRepr

        def baseClasses: List[Symbol]

        def baseType(cls: Symbol): TypeRepr

        def derivesFrom(cls: Symbol): Boolean

        def isFunctionType: Boolean

        def isContextFunctionType: Boolean

        def isErasedFunctionType: Boolean

        def isDependentFunctionType: Boolean

        def select(sym: Symbol): TypeRepr

        def appliedTo(targ: TypeRepr): TypeRepr

        def appliedTo(targs: List[TypeRepr]): TypeRepr

      end extension
    }

    type ConstantType <: TypeRepr

    given ConstantTypeTypeTest: TypeTest[TypeRepr, ConstantType]

    val ConstantType: ConstantTypeModule

    trait ConstantTypeModule { this: ConstantType.type =>
      def apply(x : Constant): ConstantType
      def unapply(x: ConstantType): Some[Constant]
    }

    given ConstantTypeMethods: ConstantTypeMethods

    trait ConstantTypeMethods:
      extension (self: ConstantType)
        def constant: Constant
      end extension
    end ConstantTypeMethods

    type NamedType <: TypeRepr

    given NamedTypeTypeTest: TypeTest[TypeRepr, NamedType]

    given NamedTypeMethods: NamedTypeMethods

    trait NamedTypeMethods:
      extension (self: NamedType)
        def qualifier: TypeRepr
        def name: String
      end extension
    end NamedTypeMethods

    type TermRef <: NamedType

    given TermRefTypeTest: TypeTest[TypeRepr, TermRef]

    val TermRef: TermRefModule

    trait TermRefModule { this: TermRef.type =>
      def apply(qual: TypeRepr, name: String): TermRef
      def unapply(x: TermRef): (TypeRepr, String)
    }

    type TypeRef <: NamedType

    given TypeRefTypeTest: TypeTest[TypeRepr, TypeRef]

    val TypeRef: TypeRefModule

    trait TypeRefModule { this: TypeRef.type =>
      def unapply(x: TypeRef): (TypeRepr, String)
    }

    given TypeRefMethods: TypeRefMethods

    trait TypeRefMethods:
      extension (self: TypeRef)
        def isOpaqueAlias: Boolean
        def translucentSuperType: TypeRepr
      end extension
    end TypeRefMethods

    type SuperType <: TypeRepr

    given SuperTypeTypeTest: TypeTest[TypeRepr, SuperType]

    val SuperType: SuperTypeModule

    trait SuperTypeModule { this: SuperType.type =>
      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
      def unapply(x: SuperType): (TypeRepr, TypeRepr)
    }

    given SuperTypeMethods: SuperTypeMethods

    trait SuperTypeMethods { this: SuperTypeMethods =>
      extension (self: SuperType)
        def thistpe: TypeRepr
        def supertpe: TypeRepr
      end extension
    }

    type Refinement <: TypeRepr

    given RefinementTypeTest: TypeTest[TypeRepr, Refinement]

    val Refinement: RefinementModule

    trait RefinementModule { this: Refinement.type =>
      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement
      def unapply(x: Refinement): (TypeRepr, String, TypeRepr)
    }

    given RefinementMethods: RefinementMethods

    trait RefinementMethods:
      extension (self: Refinement)
        def parent: TypeRepr
        def name: String
        def info: TypeRepr
      end extension
    end RefinementMethods

    type AppliedType <: TypeRepr

    given AppliedTypeTypeTest: TypeTest[TypeRepr, AppliedType]

    val AppliedType: AppliedTypeModule

    trait AppliedTypeModule { this: AppliedType.type =>
      def unapply(x: AppliedType): (TypeRepr, List[TypeRepr])
    }

    given AppliedTypeMethods: AppliedTypeMethods

    trait AppliedTypeMethods:
      extension (self: AppliedType)
        def tycon: TypeRepr
        def args: List[TypeRepr]
      end extension
    end AppliedTypeMethods

    type AnnotatedType <: TypeRepr

    given AnnotatedTypeTypeTest: TypeTest[TypeRepr, AnnotatedType]

    val AnnotatedType: AnnotatedTypeModule

    trait AnnotatedTypeModule { this: AnnotatedType.type =>
      def apply(underlying: TypeRepr, annot: Term): AnnotatedType
      def unapply(x: AnnotatedType): (TypeRepr, Term)
    }

    given AnnotatedTypeMethods: AnnotatedTypeMethods

    trait AnnotatedTypeMethods:
      extension (self: AnnotatedType)
        def underlying: TypeRepr
        def annotation: Term
      end extension
    end AnnotatedTypeMethods

    type AndOrType <: TypeRepr

    given AndOrTypeTypeTest: TypeTest[TypeRepr, AndOrType]

    given AndOrTypeMethods: AndOrTypeMethods

    trait AndOrTypeMethods:
      extension (self: AndOrType)
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end AndOrTypeMethods

    type AndType <: AndOrType

    given AndTypeTypeTest: TypeTest[TypeRepr, AndType]

    val AndType: AndTypeModule

    trait AndTypeModule { this: AndType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): AndType
      def unapply(x: AndType): (TypeRepr, TypeRepr)
    }

    type OrType <: AndOrType

    given OrTypeTypeTest: TypeTest[TypeRepr, OrType]

    val OrType: OrTypeModule

    trait OrTypeModule { this: OrType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): OrType
      def unapply(x: OrType): (TypeRepr, TypeRepr)
    }

    type MatchType <: TypeRepr

    given MatchTypeTypeTest: TypeTest[TypeRepr, MatchType]

    val MatchType: MatchTypeModule

    trait MatchTypeModule { this: MatchType.type =>
      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
      def unapply(x: MatchType): (TypeRepr, TypeRepr, List[TypeRepr])
    }

    given MatchTypeMethods: MatchTypeMethods

    trait MatchTypeMethods:
      extension (self: MatchType)
        def bound: TypeRepr
        def scrutinee: TypeRepr
        def cases: List[TypeRepr]
      end extension
    end MatchTypeMethods

    type ByNameType <: TypeRepr

    given ByNameTypeTypeTest: TypeTest[TypeRepr, ByNameType]

    val ByNameType: ByNameTypeModule

    trait ByNameTypeModule { this: ByNameType.type =>
      def apply(underlying: TypeRepr): TypeRepr
      def unapply(x: ByNameType): Some[TypeRepr]
    }

    given ByNameTypeMethods: ByNameTypeMethods

    trait ByNameTypeMethods:
      extension (self: ByNameType)
        def underlying: TypeRepr
      end extension
    end ByNameTypeMethods

    type ParamRef <: TypeRepr

    given ParamRefTypeTest: TypeTest[TypeRepr, ParamRef]

    val ParamRef: ParamRefModule

    trait ParamRefModule { this: ParamRef.type =>
      def unapply(x: ParamRef): (TypeRepr, Int)
    }

    given ParamRefMethods: ParamRefMethods

    trait ParamRefMethods:
      extension (self: ParamRef)
        def binder: TypeRepr
        def paramNum: Int
      end extension
    end ParamRefMethods

    type ThisType <: TypeRepr

    given ThisTypeTypeTest: TypeTest[TypeRepr, ThisType]

    val ThisType: ThisTypeModule

    trait ThisTypeModule { this: ThisType.type =>
      def unapply(x: ThisType): Some[TypeRepr]
    }

    given ThisTypeMethods: ThisTypeMethods

    trait ThisTypeMethods:
      extension (self: ThisType)
        def tref: TypeRepr
      end extension
    end ThisTypeMethods

    type RecursiveThis <: TypeRepr

    given RecursiveThisTypeTest: TypeTest[TypeRepr, RecursiveThis]

    val RecursiveThis: RecursiveThisModule

    trait RecursiveThisModule { this: RecursiveThis.type =>
      def unapply(x: RecursiveThis): Some[RecursiveType]
    }

    given RecursiveThisMethods: RecursiveThisMethods

    trait RecursiveThisMethods:
      extension (self: RecursiveThis)
        def binder: RecursiveType
      end extension
    end RecursiveThisMethods

    type RecursiveType <: TypeRepr

    given RecursiveTypeTypeTest: TypeTest[TypeRepr, RecursiveType]

    val RecursiveType: RecursiveTypeModule

    trait RecursiveTypeModule { this: RecursiveType.type =>

      def apply(parentExp: RecursiveType => TypeRepr): RecursiveType

      def unapply(x: RecursiveType): Some[TypeRepr]
    }

    given RecursiveTypeMethods: RecursiveTypeMethods

    trait RecursiveTypeMethods:
      extension (self: RecursiveType)
        def underlying: TypeRepr
        def recThis: RecursiveThis
      end extension
    end RecursiveTypeMethods

    type LambdaType <: TypeRepr

    given LambdaTypeTypeTest: TypeTest[TypeRepr, LambdaType]

    given LambdaTypeMethods: LambdaTypeMethods

    trait LambdaTypeMethods:
      extension (self: LambdaType)
        def paramNames: List[String]
        def paramTypes: List[TypeRepr]
        def resType: TypeRepr
      end extension
    end LambdaTypeMethods

    type MethodOrPoly <: LambdaType

    given MethodOrPolyTypeTest: TypeTest[TypeRepr, MethodOrPoly]

    type MethodType <: MethodOrPoly

    given MethodTypeTypeTest: TypeTest[TypeRepr, MethodType]

    val MethodType: MethodTypeModule

    trait MethodTypeModule { this: MethodType.type =>
      def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
      def unapply(x: MethodType): (List[String], List[TypeRepr], TypeRepr)
    }

    given MethodTypeMethods: MethodTypeMethods

    trait MethodTypeMethods:
      extension (self: MethodType)
        def isImplicit: Boolean
        def isErased: Boolean
        def param(idx: Int): TypeRepr
      end extension
    end MethodTypeMethods

    type PolyType <: MethodOrPoly

    given PolyTypeTypeTest: TypeTest[TypeRepr, PolyType]

    val PolyType: PolyTypeModule

    trait PolyTypeModule { this: PolyType.type =>
      def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
      def unapply(x: PolyType): (List[String], List[TypeBounds], TypeRepr)
    }

    given PolyTypeMethods: PolyTypeMethods

    trait PolyTypeMethods:
      extension (self: PolyType)
        def param(idx: Int): TypeRepr
        def paramBounds: List[TypeBounds]
      end extension
    end PolyTypeMethods

    type TypeLambda <: LambdaType

    given TypeLambdaTypeTest: TypeTest[TypeRepr, TypeLambda]

    val TypeLambda: TypeLambdaModule

    trait TypeLambdaModule { this: TypeLambda.type =>
      def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
      def unapply(x: TypeLambda): (List[String], List[TypeBounds], TypeRepr)
    }

    given TypeLambdaMethods: TypeLambdaMethods

    trait TypeLambdaMethods:
      extension (self: TypeLambda)
        def param(idx: Int) : TypeRepr
        def paramBounds: List[TypeBounds]
      end extension
    end TypeLambdaMethods

    type MatchCase <: TypeRepr

    given MatchCaseTypeTest: TypeTest[TypeRepr, MatchCase]

    val MatchCase: MatchCaseModule

    trait MatchCaseModule { this: MatchCase.type =>
      def apply(pattern: TypeRepr, rhs: TypeRepr): MatchCase
      def unapply(x: MatchCase): (TypeRepr, TypeRepr)
    }

    given MatchCaseMethods: MatchCaseMethods

    trait MatchCaseMethods:
      extension (self: MatchCase)
        def pattern: TypeRepr
        def rhs: TypeRepr
      end extension
    end MatchCaseMethods

    type TypeBounds <: TypeRepr

    given TypeBoundsTypeTest: TypeTest[TypeRepr, TypeBounds]

    val TypeBounds: TypeBoundsModule

    trait TypeBoundsModule { this: TypeBounds.type =>
      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds
      def unapply(x: TypeBounds): (TypeRepr, TypeRepr)
      def empty: TypeBounds
      def upper(hi: TypeRepr): TypeBounds
      def lower(lo: TypeRepr): TypeBounds
    }

    given TypeBoundsMethods: TypeBoundsMethods

    trait TypeBoundsMethods:
      extension (self: TypeBounds)
        def low: TypeRepr
        def hi: TypeRepr
      end extension
    end TypeBoundsMethods

    type NoPrefix <: TypeRepr

    given NoPrefixTypeTest: TypeTest[TypeRepr, NoPrefix]

    val NoPrefix: NoPrefixModule

    trait NoPrefixModule { this: NoPrefix.type =>
      def unapply(x: NoPrefix): true
    }

    type Constant <: AnyRef

    val Constant: ConstantModule

    trait ConstantModule { this: Constant.type => }

    given ConstantMethods: ConstantMethods

    trait ConstantMethods {
      extension (self: Constant)
        def value: Any

        def show(using Printer[Constant]): String

      end extension
    }

    type BooleanConstant <: Constant

    given BooleanConstantTypeTest: TypeTest[Constant, BooleanConstant]

    val BooleanConstant: BooleanConstantModule

    trait BooleanConstantModule { this: BooleanConstant.type =>
      def apply(x: Boolean): BooleanConstant
      def unapply(constant: BooleanConstant): Some[Boolean]
    }

    type ByteConstant <: Constant

    given ByteConstantTypeTest: TypeTest[Constant, ByteConstant]

    val ByteConstant: ByteConstantModule

    trait ByteConstantModule { this: ByteConstant.type =>
      def apply(x: Byte): ByteConstant
      def unapply(constant: ByteConstant): Some[Byte]
    }

    type ShortConstant <: Constant

    given ShortConstantTypeTest: TypeTest[Constant, ShortConstant]

    val ShortConstant: ShortConstantModule

    trait ShortConstantModule { this: ShortConstant.type =>
      def apply(x: Short): ShortConstant
      def unapply(constant: ShortConstant): Some[Short]
    }

    type IntConstant <: Constant

    given IntConstantTypeTest: TypeTest[Constant, IntConstant]

    val IntConstant: IntConstantModule

    trait IntConstantModule { this: IntConstant.type =>
      def apply(x: Int): IntConstant
      def unapply(constant: IntConstant): Some[Int]
    }

    type LongConstant <: Constant

    given LongConstantTypeTest: TypeTest[Constant, LongConstant]

    val LongConstant: LongConstantModule

    trait LongConstantModule { this: LongConstant.type =>
      def apply(x: Long): LongConstant
      def unapply(constant: LongConstant): Some[Long]
    }

    type FloatConstant <: Constant

    given FloatConstantTypeTest: TypeTest[Constant, FloatConstant]

    val FloatConstant: FloatConstantModule

    trait FloatConstantModule { this: FloatConstant.type =>
      def apply(x: Float): FloatConstant
      def unapply(constant: FloatConstant): Some[Float]
    }

    type DoubleConstant <: Constant

    given DoubleConstantTypeTest: TypeTest[Constant, DoubleConstant]

    val DoubleConstant: DoubleConstantModule

    trait DoubleConstantModule { this: DoubleConstant.type =>
      def apply(x: Double): DoubleConstant
      def unapply(constant: DoubleConstant): Some[Double]
    }

    type CharConstant <: Constant

    given CharConstantTypeTest: TypeTest[Constant, CharConstant]

    val CharConstant: CharConstantModule

    trait CharConstantModule { this: CharConstant.type =>
      def apply(x: Char): CharConstant
      def unapply(constant: CharConstant): Some[Char]
    }

    type StringConstant <: Constant

    given StringConstantTypeTest: TypeTest[Constant, StringConstant]

    val StringConstant: StringConstantModule

    trait StringConstantModule { this: StringConstant.type =>
      def apply(x: String): StringConstant
      def unapply(constant: StringConstant): Some[String]
    }

    type UnitConstant <: Constant

    given UnitConstantTypeTest: TypeTest[Constant, UnitConstant]

    val UnitConstant: UnitConstantModule

    trait UnitConstantModule { this: UnitConstant.type =>
      def apply(): UnitConstant
      def unapply(constant: UnitConstant): true
    }

    type NullConstant <: Constant

    given NullConstantTypeTest: TypeTest[Constant, NullConstant]

    val NullConstant: NullConstantModule

    trait NullConstantModule { this: NullConstant.type =>
      def apply(): NullConstant
      def unapply(constant: NullConstant): Boolean
    }

    type ClassOfConstant <: Constant

    given ClassOfConstantTypeTest: TypeTest[Constant, ClassOfConstant]

    val ClassOfConstant: ClassOfConstantModule

    trait ClassOfConstantModule { this: ClassOfConstant.type =>
      def apply(tpe: TypeRepr): ClassOfConstant
      def unapply(constant: ClassOfConstant): Option[TypeRepr]
    }

    val Implicits: ImplicitsModule

    trait ImplicitsModule { self: Implicits.type =>
      def search(tpe: TypeRepr): ImplicitSearchResult
    }

    type ImplicitSearchResult <: AnyRef

    given ImplicitSearchSuccessTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]

    type ImplicitSearchSuccess <: ImplicitSearchResult

    given ImplicitSearchSuccessMethods: ImplicitSearchSuccessMethods

    trait ImplicitSearchSuccessMethods:
      extension (self: ImplicitSearchSuccess)
        def tree: Term
      end extension
    end ImplicitSearchSuccessMethods

    type ImplicitSearchFailure <: ImplicitSearchResult

    given ImplicitSearchFailureTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchFailure]

    given ImplicitSearchFailureMethods: ImplicitSearchFailureMethods

    trait ImplicitSearchFailureMethods:
      extension (self: ImplicitSearchFailure)
        def explanation: String
      end extension
    end ImplicitSearchFailureMethods

    type DivergingImplicit <: ImplicitSearchFailure

    given DivergingImplicitTypeTest: TypeTest[ImplicitSearchResult, DivergingImplicit]

    type NoMatchingImplicits <: ImplicitSearchFailure

    given NoMatchingImplicitsTypeTest: TypeTest[ImplicitSearchResult, NoMatchingImplicits]

    type AmbiguousImplicits <: ImplicitSearchFailure

    given AmbiguousImplicitsTypeTest: TypeTest[ImplicitSearchResult, AmbiguousImplicits]

    type Symbol <: AnyRef

    val Symbol: SymbolModule

    trait SymbolModule { this: Symbol.type =>

      def spliceOwner: Symbol

      def requiredPackage(path: String): Symbol

      def requiredClass(path: String): Symbol

      def requiredModule(path: String): Symbol

      def requiredMethod(path: String): Symbol

      def classSymbol(fullName: String): Symbol

      def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol

      def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol

      def noSymbol: Symbol
    }

    given SymbolMethods: SymbolMethods

    trait SymbolMethods {
      extension (self: Symbol)

        def owner: Symbol

        def maybeOwner: Symbol

        def flags: Flags

        def privateWithin: Option[TypeRepr]

        def protectedWithin: Option[TypeRepr]

        def name: String

        def fullName: String

        def pos: Option[Position]

        def docstring: Option[String]

        def tree: Tree

        def hasAnnotation(annotSym: Symbol): Boolean

        def getAnnotation(annotSym: Symbol): Option[Term]

        def annotations: List[Term]

        def isDefinedInCurrentRun: Boolean

        def isLocalDummy: Boolean

        def isRefinementClass: Boolean

        def isAliasType: Boolean

        def isAnonymousClass: Boolean

        def isAnonymousFunction: Boolean

        def isAbstractType: Boolean

        def isClassConstructor: Boolean

        def isType: Boolean

        def isTerm: Boolean

        def isPackageDef: Boolean

        def isClassDef: Boolean

        def isTypeDef: Boolean

        def isValDef: Boolean

        def isDefDef: Boolean

        def isBind: Boolean

        def isNoSymbol: Boolean

        def exists: Boolean

        def declaredField(name: String): Symbol

        def declaredFields: List[Symbol]

        def memberField(name: String): Symbol

        def memberFields: List[Symbol]

        def declaredMethod(name: String): List[Symbol]

        def declaredMethods: List[Symbol]

        def memberMethod(name: String): List[Symbol]

        def memberMethods: List[Symbol]

        def declaredType(name: String): List[Symbol]

        def declaredTypes: List[Symbol]

        def memberType(name: String): Symbol

        def memberTypes: List[Symbol]

        def declarations: List[Symbol]

        def paramSymss: List[List[Symbol]]

        def allOverriddenSymbols: Iterator[Symbol]

        def overridingSymbol(ofclazz: Symbol): Symbol

        def primaryConstructor: Symbol

        def caseFields: List[Symbol]

        def isTypeParam: Boolean

        def signature: Signature

        def moduleClass: Symbol

        def companionClass: Symbol

        def companionModule: Symbol

        def children: List[Symbol]
      end extension
    }

    type Signature <: AnyRef

    val Signature: SignatureModule

    trait SignatureModule { this: Signature.type =>
      def unapply(sig: Signature): (List[String | Int], String)
    }

    given SignatureMethods: SignatureMethods

    trait SignatureMethods {
      extension (self: Signature)

        def paramSigs: List[String | Int]

        def resultSig: String

      end extension
    }

    val defn: defnModule

    trait defnModule { self: defn.type =>

      def RootPackage: Symbol

      def RootClass: Symbol

      def EmptyPackageClass: Symbol

      def ScalaPackage: Symbol

      def ScalaPackageClass: Symbol

      def AnyClass: Symbol

      def MatchableClass: Symbol

      def AnyValClass: Symbol

      def ObjectClass: Symbol

      def AnyRefClass: Symbol

      def NullClass: Symbol

      def NothingClass: Symbol

      def UnitClass: Symbol

      def ByteClass: Symbol

      def ShortClass: Symbol

      def CharClass: Symbol

      def IntClass: Symbol

      def LongClass: Symbol

      def FloatClass: Symbol

      def DoubleClass: Symbol

      def BooleanClass: Symbol

      def StringClass: Symbol

      def ClassClass: Symbol

      def ArrayClass: Symbol

      def PredefModule: Symbol

      def Predef_classOf: Symbol

      def JavaLangPackage: Symbol

      def ArrayModule: Symbol

      def Array_apply: Symbol

      def Array_clone: Symbol

      def Array_length: Symbol

      def Array_update: Symbol

      def RepeatedParamClass: Symbol

      def RepeatedAnnot: Symbol

      def OptionClass: Symbol

      def NoneModule: Symbol

      def SomeModule: Symbol

      def ProductClass: Symbol

      def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

      def TupleClass(arity: Int): Symbol

      def isTupleClass(sym: Symbol): Boolean

      def ScalaPrimitiveValueClasses: List[Symbol]

      def ScalaNumericValueClasses: List[Symbol]

    }

    type Flags

    val Flags: FlagsModule

    trait FlagsModule { this: Flags.type =>

      def Abstract: Flags

      def Artifact: Flags

      def Case: Flags

      def CaseAccessor: Flags

      def Contravariant: Flags

      def Covariant: Flags

      def Deferred: Flags

      def EmptyFlags: Flags

      def Enum: Flags

      def Erased: Flags

      def Exported: Flags

      def ExtensionMethod: Flags

      def FieldAccessor: Flags

      def Final: Flags

      def Given: Flags

      def HasDefault: Flags

      def Implicit: Flags

      def Infix: Flags

      def Inline: Flags

      def JavaDefined: Flags

      def JavaStatic: Flags

      def Lazy: Flags

      def Local: Flags

      def Macro: Flags

      def Method: Flags

      def Module: Flags

      def Mutable: Flags

      def NoInits: Flags

      def Opaque: Flags

      def Open: Flags

      def Override: Flags

      def Package: Flags

      def Param: Flags

      def ParamAccessor: Flags

      def Private: Flags

      def PrivateLocal: Flags

      def Protected: Flags

      def Scala2x: Flags

      def Sealed: Flags

      def StableRealizable: Flags

      def Static: Flags

      def Synthetic: Flags

      def Trait: Flags

      def Transparent: Flags

    }

    given FlagsMethods: FlagsMethods

    trait FlagsMethods {
      extension (self: Flags)
        def is(that: Flags): Boolean

        def |(that: Flags): Flags

        def &(that: Flags): Flags

        def show: String

      end extension
    }

    type Position <: AnyRef

    val Position: PositionModule

    trait PositionModule { this: Position.type =>
      def ofMacroExpansion: Position

      def apply(sourceFile: SourceFile, start: Int, end: Int): Position
    }

    given PositionMethods: PositionMethods

    trait PositionMethods {
      extension (self: Position)

        def start: Int

        def end: Int

        def sourceFile: SourceFile

        def startLine: Int

        def endLine: Int

        def startColumn: Int

        def endColumn: Int

        def sourceCode: Option[String]

      end extension
    }

    type SourceFile <: AnyRef

    val SourceFile: SourceFileModule

    trait SourceFileModule { this: SourceFile.type =>
      def current: SourceFile
    }

    given SourceFileMethods: SourceFileMethods

    trait SourceFileMethods {
      extension (self: SourceFile)
        def jpath: java.nio.file.Path

        def content: Option[String]
      end extension
    }

    val report: reportModule

    trait reportModule { self: report.type =>

      def error(msg: String): Unit

      def error(msg: String, expr: Expr[Any]): Unit

      def error(msg: String, pos: Position): Unit

      def throwError(msg: String): Nothing

      def throwError(msg: String, expr: Expr[Any]): Nothing

      def throwError(msg: String, pos: Position): Nothing

      def warning(msg: String): Unit

      def warning(msg: String, expr: Expr[Any]): Unit

      def warning(msg: String, pos: Position): Unit

    }

    trait TreeAccumulator[X]:

      def foldTree(x: X, tree: Tree)(owner: Symbol): X

      def foldTrees(x: X, trees: Iterable[Tree])(owner: Symbol): X = trees.foldLeft(x)((acc, y) => foldTree(acc, y)(owner))

      def foldOverTree(x: X, tree: Tree)(owner: Symbol): X = {
        tree match {
          case Ident(_) =>
            x
          case Select(qualifier, _) =>
            foldTree(x, qualifier)(owner)
          case This(qual) =>
            x
          case Super(qual, _) =>
            foldTree(x, qual)(owner)
          case Apply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case TypeApply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case Literal(const) =>
            x
          case New(tpt) =>
            foldTree(x, tpt)(owner)
          case Typed(expr, tpt) =>
            foldTree(foldTree(x, expr)(owner), tpt)(owner)
          case NamedArg(_, arg) =>
            foldTree(x, arg)(owner)
          case Assign(lhs, rhs) =>
            foldTree(foldTree(x, lhs)(owner), rhs)(owner)
          case Block(stats, expr) =>
            foldTree(foldTrees(x, stats)(owner), expr)(owner)
          case If(cond, thenp, elsep) =>
            foldTree(foldTree(foldTree(x, cond)(owner), thenp)(owner), elsep)(owner)
          case While(cond, body) =>
            foldTree(foldTree(x, cond)(owner), body)(owner)
          case Closure(meth, tpt) =>
            foldTree(x, meth)(owner)
          case Match(selector, cases) =>
            foldTrees(foldTree(x, selector)(owner), cases)(owner)
          case Return(expr, _) =>
            foldTree(x, expr)(owner)
          case Try(block, handler, finalizer) =>
            foldTrees(foldTrees(foldTree(x, block)(owner), handler)(owner), finalizer)(owner)
          case Repeated(elems, elemtpt) =>
            foldTrees(foldTree(x, elemtpt)(owner), elems)(owner)
          case Inlined(call, bindings, expansion) =>
            foldTree(foldTrees(x, bindings)(owner), expansion)(owner)
          case vdef @ ValDef(_, tpt, rhs) =>
            val owner = vdef.symbol
            foldTrees(foldTree(x, tpt)(owner), rhs)(owner)
          case ddef @ DefDef(_, tparams, vparamss, tpt, rhs) =>
            val owner = ddef.symbol
            foldTrees(foldTree(vparamss.foldLeft(foldTrees(x, tparams)(owner))((acc, y) => foldTrees(acc, y)(owner)), tpt)(owner), rhs)(owner)
          case tdef @ TypeDef(_, rhs) =>
            val owner = tdef.symbol
            foldTree(x, rhs)(owner)
          case cdef @ ClassDef(_, constr, parents, derived, self, body) =>
            val owner = cdef.symbol
            foldTrees(foldTrees(foldTrees(foldTrees(foldTree(x, constr)(owner), parents)(owner), derived)(owner), self)(owner), body)(owner)
          case Import(expr, _) =>
            foldTree(x, expr)(owner)
          case Export(expr, _) =>
            foldTree(x, expr)(owner)
          case clause @ PackageClause(pid, stats) =>
            foldTrees(foldTree(x, pid)(owner), stats)(clause.symbol)
          case Inferred() => x
          case TypeIdent(_) => x
          case TypeSelect(qualifier, _) => foldTree(x, qualifier)(owner)
          case TypeProjection(qualifier, _) => foldTree(x, qualifier)(owner)
          case Singleton(ref) => foldTree(x, ref)(owner)
          case Refined(tpt, refinements) => foldTrees(foldTree(x, tpt)(owner), refinements)(owner)
          case Applied(tpt, args) => foldTrees(foldTree(x, tpt)(owner), args)(owner)
          case ByName(result) => foldTree(x, result)(owner)
          case Annotated(arg, annot) => foldTree(foldTree(x, arg)(owner), annot)(owner)
          case LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs)(owner), arg)(owner)
          case TypeBind(_, tbt) => foldTree(x, tbt)(owner)
          case TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs)(owner), tpt)(owner)
          case MatchTypeTree(boundopt, selector, cases) =>
            foldTrees(foldTree(boundopt.fold(x)(y => foldTree(x, y)(owner)), selector)(owner), cases)(owner)
          case WildcardTypeTree() => x
          case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo)(owner), hi)(owner)
          case CaseDef(pat, guard, body) => foldTree(foldTrees(foldTree(x, pat)(owner), guard)(owner), body)(owner)
          case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat)(owner), body)(owner)
          case Bind(_, body) => foldTree(x, body)(owner)
          case Unapply(fun, implicits, patterns) => foldTrees(foldTrees(foldTree(x, fun)(owner), implicits)(owner), patterns)(owner)
          case Alternatives(patterns) => foldTrees(x, patterns)(owner)
        }
      }
    end TreeAccumulator

    trait TreeTraverser extends TreeAccumulator[Unit]:

      def traverseTree(tree: Tree)(owner: Symbol): Unit = traverseTreeChildren(tree)(owner)

      def foldTree(x: Unit, tree: Tree)(owner: Symbol): Unit = traverseTree(tree)(owner)

      protected def traverseTreeChildren(tree: Tree)(owner: Symbol): Unit = foldOverTree((), tree)(owner)

    end TreeTraverser

    trait TreeMap:

      def transformTree(tree: Tree)(owner: Symbol): Tree = {
        tree match {
          case tree: PackageClause =>
            PackageClause.copy(tree)(transformTerm(tree.pid).asInstanceOf[Ref], transformTrees(tree.stats)(tree.symbol))
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Export =>
            tree
          case tree: Statement =>
            transformStatement(tree)(owner)
          case tree: TypeTree => transformTypeTree(tree)(owner)
          case tree: TypeBoundsTree =>
            TypeBoundsTree.copy(tree)(transformTypeTree(tree.low)(owner), transformTypeTree(tree.hi)(owner))
          case tree: WildcardTypeTree => tree
          case tree: CaseDef =>
            transformCaseDef(tree)(owner)
          case tree: TypeCaseDef =>
            transformTypeCaseDef(tree)(owner)
          case pattern: Bind =>
            Bind.copy(pattern)(pattern.name, pattern.pattern)
          case pattern: Unapply =>
            Unapply.copy(pattern)(transformTerm(pattern.fun)(owner), transformSubTrees(pattern.implicits)(owner), transformTrees(pattern.patterns)(owner))
          case pattern: Alternatives =>
            Alternatives.copy(pattern)(transformTrees(pattern.patterns)(owner))
        }
      }

      def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case tree: Term =>
            transformTerm(tree)(owner)
          case tree: ValDef =>
            val owner = tree.symbol
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))
            ValDef.copy(tree)(tree.name, tpt1, rhs1)
          case tree: DefDef =>
            val owner = tree.symbol
            DefDef.copy(tree)(tree.name, transformSubTrees(tree.typeParams)(owner), tree.paramss mapConserve (x => transformSubTrees(x)(owner)), transformTypeTree(tree.returnTpt)(owner), tree.rhs.map(x => transformTerm(x)(owner)))
          case tree: TypeDef =>
            val owner = tree.symbol
            TypeDef.copy(tree)(tree.name, transformTree(tree.rhs)(owner))
          case tree: ClassDef =>
            ClassDef.copy(tree)(tree.name, tree.constructor, tree.parents, tree.derived, tree.self, tree.body)
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Export =>
            tree
        }
      }

      def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case Ident(name) =>
            tree
          case Select(qualifier, name) =>
            Select.copy(tree)(transformTerm(qualifier)(owner), name)
          case This(qual) =>
            tree
          case Super(qual, mix) =>
            Super.copy(tree)(transformTerm(qual)(owner), mix)
          case Apply(fun, args) =>
            Apply.copy(tree)(transformTerm(fun)(owner), transformTerms(args)(owner))
          case TypeApply(fun, args) =>
            TypeApply.copy(tree)(transformTerm(fun)(owner), transformTypeTrees(args)(owner))
          case Literal(const) =>
            tree
          case New(tpt) =>
            New.copy(tree)(transformTypeTree(tpt)(owner))
          case Typed(expr, tpt) =>
            Typed.copy(tree)(transformTerm(expr)(owner), transformTypeTree(tpt)(owner))
          case tree: NamedArg =>
            NamedArg.copy(tree)(tree.name, transformTerm(tree.value)(owner))
          case Assign(lhs, rhs) =>
            Assign.copy(tree)(transformTerm(lhs)(owner), transformTerm(rhs)(owner))
          case Block(stats, expr) =>
            Block.copy(tree)(transformStats(stats)(owner), transformTerm(expr)(owner))
          case If(cond, thenp, elsep) =>
            If.copy(tree)(transformTerm(cond)(owner), transformTerm(thenp)(owner), transformTerm(elsep)(owner))
          case Closure(meth, tpt) =>
            Closure.copy(tree)(transformTerm(meth)(owner), tpt)
          case Match(selector, cases) =>
            Match.copy(tree)(transformTerm(selector)(owner), transformCaseDefs(cases)(owner))
          case Return(expr, from) =>
            Return.copy(tree)(transformTerm(expr)(owner), from)
          case While(cond, body) =>
            While.copy(tree)(transformTerm(cond)(owner), transformTerm(body)(owner))
          case Try(block, cases, finalizer) =>
            Try.copy(tree)(transformTerm(block)(owner), transformCaseDefs(cases)(owner), finalizer.map(x => transformTerm(x)(owner)))
          case Repeated(elems, elemtpt) =>
            Repeated.copy(tree)(transformTerms(elems)(owner), transformTypeTree(elemtpt)(owner))
          case Inlined(call, bindings, expansion) =>
            Inlined.copy(tree)(call, transformSubTrees(bindings)(owner), transformTerm(expansion)(owner))
        }
      }

      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree match {
        case Inferred() => tree
        case tree: TypeIdent => tree
        case tree: TypeSelect =>
          TypeSelect.copy(tree)(tree.qualifier, tree.name)
        case tree: TypeProjection =>
          TypeProjection.copy(tree)(tree.qualifier, tree.name)
        case tree: Annotated =>
          Annotated.copy(tree)(tree.arg, tree.annotation)
        case tree: Singleton =>
          Singleton.copy(tree)(transformTerm(tree.ref)(owner))
        case tree: Refined =>
          Refined.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.refinements)(owner).asInstanceOf[List[Definition]])
        case tree: Applied =>
          Applied.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.args)(owner))
        case tree: MatchTypeTree =>
          MatchTypeTree.copy(tree)(tree.bound.map(b => transformTypeTree(b)(owner)), transformTypeTree(tree.selector)(owner), transformTypeCaseDefs(tree.cases)(owner))
        case tree: ByName =>
          ByName.copy(tree)(transformTypeTree(tree.result)(owner))
        case tree: LambdaTypeTree =>
          LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams)(owner), transformTree(tree.body)(owner))
        case tree: TypeBind =>
          TypeBind.copy(tree)(tree.name, tree.body)
        case tree: TypeBlock =>
          TypeBlock.copy(tree)(tree.aliases, tree.tpt)
      }

      def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef = {
        CaseDef.copy(tree)(transformTree(tree.pattern)(owner), tree.guard.map(x => transformTerm(x)(owner)), transformTerm(tree.rhs)(owner))
      }

      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))
      }

      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees mapConserve (x => transformStatement(x)(owner))

      def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        trees mapConserve (x => transformTree(x)(owner))

      def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        trees mapConserve (x => transformTerm(x)(owner))

      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees mapConserve (x => transformTypeTree(x)(owner))

      def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x)(owner))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees mapConserve (x => transformTypeCaseDef(x)(owner))

      def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        transformTrees(trees)(owner).asInstanceOf[List[Tr]]

    end TreeMap

    trait Printer[T]:
      def show(x: T): String
    end Printer

    given TreePrinter: Printer[Tree] = Printer.TreeCode

    given TypeReprPrinter: Printer[TypeRepr] = Printer.TypeReprCode

    given ConstantPrinter: Printer[Constant] = Printer.ConstantCode

    val Printer: PrinterModule

    trait PrinterModule { self: Printer.type =>
      def TreeCode: Printer[Tree]

      def TreeShortCode: Printer[Tree]

      def TreeAnsiCode: Printer[Tree]

      def TreeStructure: Printer[Tree]

      def TypeReprCode: Printer[TypeRepr]

      def TypeReprShortCode: Printer[TypeRepr]

      def TypeReprAnsiCode: Printer[TypeRepr]

      def TypeReprStructure: Printer[TypeRepr]

      def ConstantCode: Printer[Constant]

      def ConstantStructure: Printer[Constant]
    }

  }

  type Nested = Quotes

}
