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
