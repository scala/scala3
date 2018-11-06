package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Types

trait ReflectionCoreImpl extends scala.tasty.reflect.ReflectionCore {

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

  type CaseDef = tpd.CaseDef

  type Pattern = tpd.Tree
    type Value = tpd.Tree
    type Bind = tpd.Bind
    type Unapply = tpd.Tree
    type Alternative = tpd.Alternative
    type TypeTest = tpd.Typed

  type TypeOrBoundsTree = tpd.Tree
    type TypeTree = tpd.Tree
    type TypeBoundsTree = tpd.Tree

  type TypeOrBounds = Types.Type
    type NoPrefix = Types.NoPrefix.type
    type TypeBounds = Types.TypeBounds
    type Type = Types.Type
    type RecursiveType = Types.RecType
    type LambdaType[ParamInfo] = Types.LambdaType { type PInfo = ParamInfo }
      type MethodType = Types.MethodType
      type PolyType = Types.PolyType
      type TypeLambda = Types.TypeLambda

  type ImportSelector = untpd.Tree

  type Id = untpd.Ident

  type Signature = core.Signature

  type Position = util.SourcePosition

  type Constant = Constants.Constant

  type Symbol = core.Symbols.Symbol
    type PackageSymbol = core.Symbols.Symbol
    type ClassSymbol = core.Symbols.ClassSymbol
    type TypeSymbol = core.Symbols.TypeSymbol
    type DefSymbol = core.Symbols.TermSymbol
    type BindSymbol = core.Symbols.TermSymbol
    type ValSymbol = core.Symbols.TermSymbol
    type NoSymbol = core.Symbols.NoSymbol.type
}
