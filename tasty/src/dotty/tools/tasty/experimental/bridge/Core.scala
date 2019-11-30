package dotty.tools.tasty.experimental.bridge

trait Core with

  private[tasty] val internal: TastyKernel

  type Context = internal.Context

  type SourceFile = internal.SourceFile

  type Annotation = internal.Annotation

  type Designator = internal.Designator

  type Name = internal.Name
  type SimpleName = internal.SimpleName
  type DerivedName = internal.DerivedName
  type TypeName = internal.TypeName
  type TermName = internal.TermName

  type Signature = internal.Signature

  type Positioned = internal.Positioned

  type Tree = internal.Tree
  type MemberDef = internal.MemberDef
  type Hole = internal.Hole
  type Template = internal.Template
  type ValOrDefDef = internal.ValOrDefDef
  type TypeDef = internal.TypeDef
  type ValDef = internal.ValDef
  type DefDef = internal.DefDef
  type RefTree = internal.RefTree
  type Ident = internal.Ident
  type Select = internal.Select
  type This = internal.This
  type Apply = internal.Apply
  type TypeApply = internal.TypeApply
  type Literal = internal.Literal
  type Super = internal.Super
  type New = internal.New
  type Typed = internal.Typed
  type NamedArg = internal.NamedArg
  type Assign = internal.Assign
  type Block = internal.Block
  type If = internal.If
  type Closure = internal.Closure
  type Match = internal.Match
  type CaseDef = internal.CaseDef
  type Labeled = internal.Labeled
  type Return = internal.Return
  type WhileDo = internal.WhileDo
  type Try = internal.Try
  type SeqLiteral = internal.SeqLiteral
  type Inlined = internal.Inlined
  type Bind = internal.Bind
  type Alternative = internal.Alternative
  type UnApply = internal.UnApply
  type Import = internal.Import
  type PackageDef = internal.PackageDef
  type TypeTree = internal.TypeTree
  type SingletonTypeTree = internal.SingletonTypeTree
  type RefinedTypeTree = internal.RefinedTypeTree
  type AppliedTypeTree = internal.AppliedTypeTree
  type MatchTypeTree = internal.MatchTypeTree
  type ByNameTypeTree = internal.ByNameTypeTree
  type Annotated = internal.Annotated
  type LambdaTypeTree = internal.LambdaTypeTree
  type TypeBoundsTree = internal.TypeBoundsTree
  type Thicket = internal.Thicket

  type Type = internal.Type
  type AppliedType = internal.AppliedType
  type ConstantType = internal.ConstantType
  type NamedType = internal.NamedType
  type ThisType = internal.ThisType
  type SuperType = internal.SuperType
  type BoundType = internal.BoundType
  type RecThis = internal.RecThis
  type ParamRef = internal.ParamRef
  type RecType = internal.RecType
  type TermRef = internal.TermRef
  type TypeRef = internal.TypeRef
  type SkolemType = internal.SkolemType
  type RefinedType = internal.RefinedType
  type TypeAlias = internal.TypeAlias
  type TypeBounds = internal.TypeBounds
  type AnnotatedType = internal.AnnotatedType
  type AndOrType = internal.AndOrType
  type AndType = internal.AndType
  type OrType = internal.OrType
  type ExprType = internal.ExprType
  type TypeProxy = internal.TypeProxy
  type MatchType = internal.MatchType
  type PolyType = internal.PolyType
  type LambdaType = internal.LambdaType
  type HKTypeLambda = internal.HKTypeLambda
  type MethodType = internal.MethodType
  type LazyRef = internal.LazyRef
  type ClassInfo = internal.ClassInfo

  type Symbol = internal.Symbol
  type TermSymbol = internal.TermSymbol
  type TypeSymbol = internal.TypeSymbol
  type ClassSymbol = internal.ClassSymbol

  type FlagSet = internal.FlagSet
  type Flag = internal.Flag

  type SourcePosition = internal.SourcePosition
  type Span = internal.Span

  type Constant = internal.Constant

  type ContextDocstrings = internal.ContextDocstrings

  type Comment = internal.Comment
