package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Types

import scala.tasty.reflect.Kernel

class KernelImpl extends Kernel {
  type Context = core.Contexts.Context

  type Settings = config.ScalaSettings

  type TermOrTypeTree = tpd.Tree

  type Tree = tpd.Tree
  type PackageClause = tpd.PackageDef
  type Statement = tpd.Tree
  type Import = tpd.Import
  type Definition = tpd.Tree
  type PackageDef = PackageDefinition
  type ClassDef = tpd.TypeDef
  type TypeDef = tpd.TypeDef
  type DefDef = tpd.DefDef
  type ValDef = tpd.ValDef
  type Term = tpd.Tree
  type Term_Ref = tpd.RefTree
  type Term_Ident = tpd.Ident
  type Term_Select = tpd.Select
  type Term_Literal = tpd.Literal
  type Term_This = tpd.This
  type Term_New = tpd.New
  type Term_NamedArg = tpd.NamedArg
  type Term_Apply = tpd.Apply
  type Term_TypeApply = tpd.TypeApply
  type Term_Super = tpd.Super
  type Term_Typed = tpd.Typed
  type Term_Assign = tpd.Assign
  type Term_Block = tpd.Block
  type Term_Lambda = tpd.Closure
  type Term_If = tpd.If
  type Term_Match = tpd.Match
  type Term_Try = tpd.Try
  type Term_Return = tpd.Return
  type Term_Repeated = tpd.SeqLiteral
  type Term_Inlined = tpd.Inlined
  type Term_SelectOuter = tpd.Select
  type Term_While = tpd.WhileDo

  type CaseDef = tpd.CaseDef
  type TypeCaseDef = tpd.CaseDef

  type Pattern = tpd.Tree
  type Value = tpd.Tree
  type Bind = tpd.Bind
  type Unapply = tpd.UnApply
  type Alternatives = tpd.Alternative
  type TypeTest = tpd.Typed

  type TypeOrBoundsTree = tpd.Tree
  type TypeTree = tpd.Tree
  type TypeTree_Inferred = tpd.TypeTree
  type TypeTree_Ident = tpd.Ident
  type TypeTree_Select = tpd.Select
  type TypeTree_Projection = tpd.Select
  type TypeTree_Singleton = tpd.SingletonTypeTree
  type TypeTree_Refined = tpd.RefinedTypeTree
  type TypeTree_Applied = tpd.AppliedTypeTree
  type TypeTree_Annotated = tpd.Annotated
  type TypeTree_MatchType = tpd.MatchTypeTree
  type TypeTree_ByName = tpd.ByNameTypeTree
  type TypeTree_LambdaTypeTree = tpd.LambdaTypeTree
  type TypeTree_TypeBind = tpd.Bind
  type TypeTree_TypeBlock = tpd.Block

  type TypeBoundsTree = tpd.TypeBoundsTree
  type WildcardType = tpd.TypeTree

  type TypeOrBounds = Types.Type
  type NoPrefix = Types.NoPrefix.type
  type TypeBounds = Types.TypeBounds
  type Type = Types.Type
  type ConstantType = Types.ConstantType
  type SymRef = Types.NamedType
  type TermRef = Types.NamedType
  type TypeRef = Types.NamedType
  type SuperType = Types.SuperType
  type Refinement = Types.RefinedType
  type AppliedType = Types.AppliedType
  type AnnotatedType = Types.AnnotatedType
  type AndType = Types.AndType
  type OrType = Types.OrType
  type MatchType = Types.MatchType
  type ByNameType = Types.ExprType
  type ParamRef = Types.ParamRef
  type ThisType = Types.ThisType
  type RecursiveThis = Types.RecThis
  type RecursiveType = Types.RecType
  type LambdaType[ParamInfo] = Types.LambdaType { type PInfo = ParamInfo }
  type MethodType = Types.MethodType
  type PolyType = Types.PolyType
  type TypeLambda = Types.TypeLambda

  type ImportSelector = untpd.Tree

  type Id = untpd.Ident

  type Signature = core.Signature

  type Position = util.SourcePosition

  type Comment = core.Comments.Comment

  type Constant = Constants.Constant

  type Symbol = core.Symbols.Symbol
  type PackageSymbol = core.Symbols.Symbol
  type ClassSymbol = core.Symbols.ClassSymbol
  type TypeSymbol = core.Symbols.TypeSymbol
  type DefSymbol = core.Symbols.TermSymbol
  type BindSymbol = core.Symbols.TermSymbol
  type ValSymbol = core.Symbols.TermSymbol
  type NoSymbol = core.Symbols.NoSymbol.type

  type Flags = core.Flags.FlagSet
}
