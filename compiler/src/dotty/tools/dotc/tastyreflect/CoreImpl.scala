package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Types

trait CoreImpl extends scala.tasty.reflect.Core {

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
      val Term: TermCoreModuleImpl
      trait TermCoreModuleImpl extends TermCoreModule {
        type Ref = tpd.RefTree
          type Ident = tpd.Ident
          type Select = tpd.Select
        type Literal = tpd.Literal
        type This = tpd.This
        type New = tpd.New
        type NamedArg = tpd.NamedArg
        type Apply = tpd.Apply
        type TypeApply = tpd.TypeApply
        type Super = tpd.Super
        type Typed = tpd.Typed
        type Assign = tpd.Assign
        type Block = tpd.Block
        type Lambda = tpd.Closure
        type If = tpd.If
        type Match = tpd.Match
        type Try = tpd.Try
        type Return = tpd.Return
        type Repeated = tpd.SeqLiteral
        type Inlined = tpd.Inlined
        type SelectOuter = tpd.Select
        type While = tpd.WhileDo
      }


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
    val TypeTree: TypeTreeCoreModuleImpl
    trait TypeTreeCoreModuleImpl extends TypeTreeCoreModule {
      type Inferred = tpd.TypeTree
      type Ident = tpd.Ident
      type Select = tpd.Select
      type Projection = tpd.Select
      type Singleton = tpd.SingletonTypeTree
      type Refined = tpd.RefinedTypeTree
      type Applied = tpd.AppliedTypeTree
      type Annotated = tpd.Annotated
      type MatchType = tpd.MatchTypeTree
      type ByName = tpd.ByNameTypeTree
      type LambdaTypeTree = tpd.LambdaTypeTree
      type TypeBind = tpd.Bind
      type TypeBlock = tpd.Block
    }
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
