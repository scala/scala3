package scala.quoted.compiletime

import scala.quoted.compiletime as pub

object reflect {

  trait Module {

    lazy val Symbol: pub.Symbol.Module
    lazy val Position: pub.Position.Module
    lazy val SourceFile: pub.SourceFile.Module
    lazy val Signature: pub.Signature.Module
    lazy val Flags: pub.Flags.Module

    lazy val Selector: pub.Selector.Module
    lazy val SimpleSelector: pub.SimpleSelector.Module
    lazy val RenameSelector: pub.RenameSelector.Module
    lazy val OmitSelector: pub.OmitSelector.Module
    lazy val GivenSelector: pub.GivenSelector.Module

    lazy val ParamClause: pub.ParamClause.Module
    lazy val TermParamClause: pub.TermParamClause.Module
    lazy val TypeParamClause: pub.TypeParamClause.Module

    lazy val Constant: pub.Constant.Module
    lazy val BooleanConstant: pub.BooleanConstant.Module
    lazy val ByteConstant: pub.ByteConstant.Module
    lazy val ShortConstant: pub.ShortConstant.Module
    lazy val IntConstant: pub.IntConstant.Module
    lazy val LongConstant: pub.LongConstant.Module
    lazy val FloatConstant: pub.FloatConstant.Module
    lazy val DoubleConstant: pub.DoubleConstant.Module
    lazy val CharConstant: pub.CharConstant.Module
    lazy val StringConstant: pub.StringConstant.Module
    lazy val UnitConstant: pub.UnitConstant.Module
    lazy val NullConstant: pub.NullConstant.Module
    lazy val ClassOfConstant: pub.ClassOfConstant.Module

    lazy val TypeRepr: pub.TypeRepr.Module
    lazy val NamedType: pub.NamedType.Module
    lazy val TermRef: pub.TermRef.Module
    lazy val TypeRef: pub.TypeRef.Module
    lazy val ConstantType: pub.ConstantType.Module
    lazy val SuperType: pub.SuperType.Module
    lazy val Refinement: pub.Refinement.Module
    lazy val AppliedType: pub.AppliedType.Module
    lazy val AnnotatedType: pub.AnnotatedType.Module
    lazy val AndOrType: pub.AndOrType.Module
    lazy val AndType: pub.AndType.Module
    lazy val OrType: pub.OrType.Module
    lazy val MatchType: pub.MatchType.Module
    lazy val ByNameType: pub.ByNameType.Module
    lazy val ParamRef: pub.ParamRef.Module
    lazy val ThisType: pub.ThisType.Module
    lazy val RecursiveThis: pub.RecursiveThis.Module
    lazy val RecursiveType: pub.RecursiveType.Module
    lazy val LambdaType: pub.LambdaType.Module
    lazy val MethodOrPoly: pub.MethodOrPoly.Module
    lazy val MethodType: pub.MethodType.Module
    lazy val PolyType: pub.PolyType.Module
    lazy val TypeLambda: pub.TypeLambda.Module
    lazy val MatchCase: pub.MatchCase.Module
    lazy val TypeBounds: pub.TypeBounds.Module
    lazy val NoPrefix: pub.NoPrefix.Module
    lazy val FlexibleType: pub.FlexibleType.Module

    lazy val Tree: pub.Tree.Module
    lazy val PackageClause: pub.PackageClause.Module
    lazy val Statement: pub.Statement.Module
    lazy val Import: pub.Import.Module
    lazy val Export: pub.Export.Module
    lazy val Definition: pub.Definition.Module
    lazy val ClassDef: pub.ClassDef.Module
    lazy val ValOrDefDef: pub.ValOrDefDef.Module
    lazy val DefDef: pub.DefDef.Module
    lazy val ValDef: pub.ValDef.Module
    lazy val TypeDef: pub.TypeDef.Module
    lazy val Term: pub.Term.Module
    lazy val Ref: pub.Ref.Module
    lazy val Ident: pub.Ident.Module
    lazy val Wildcard: pub.Wildcard.Module
    lazy val Select: pub.Select.Module
    lazy val Literal: pub.Literal.Module
    lazy val This: pub.This.Module
    lazy val New: pub.New.Module
    lazy val NamedArg: pub.NamedArg.Module
    lazy val Apply: pub.Apply.Module
    lazy val TypeApply: pub.TypeApply.Module
    lazy val Super: pub.Super.Module
    lazy val Typed: pub.Typed.Module
    lazy val Assign: pub.Assign.Module
    lazy val Block: pub.Block.Module
    lazy val Closure: pub.Closure.Module
    lazy val Lambda: pub.Lambda.Module
    lazy val If: pub.If.Module
    lazy val Match: pub.Match.Module
    lazy val SummonFrom: pub.SummonFrom.Module
    lazy val Try: pub.Try.Module
    lazy val Return: pub.Return.Module
    lazy val Repeated: pub.Repeated.Module
    lazy val Inlined: pub.Inlined.Module
    lazy val SelectOuter: pub.SelectOuter.Module
    lazy val While: pub.While.Module
    lazy val TypedOrTest: pub.TypedOrTest.Module
    lazy val TypeTree: pub.TypeTree.Module
    lazy val Inferred: pub.Inferred.Module
    lazy val TypeIdent: pub.TypeIdent.Module
    lazy val TypeSelect: pub.TypeSelect.Module
    lazy val TypeProjection: pub.TypeProjection.Module
    lazy val Singleton: pub.Singleton.Module
    lazy val Refined: pub.Refined.Module
    lazy val Applied: pub.Applied.Module
    lazy val Annotated: pub.Annotated.Module
    lazy val MatchTypeTree: pub.MatchTypeTree.Module
    lazy val ByName: pub.ByName.Module
    lazy val LambdaTypeTree: pub.LambdaTypeTree.Module
    lazy val TypeBind: pub.TypeBind.Module
    lazy val TypeBlock: pub.TypeBlock.Module
    lazy val TypeBoundsTree: pub.TypeBoundsTree.Module
    lazy val WildcardTypeTree: pub.WildcardTypeTree.Module
    lazy val CaseDef: pub.CaseDef.Module
    lazy val TypeCaseDef: pub.TypeCaseDef.Module
    lazy val Bind: pub.Bind.Module
    lazy val Unapply: pub.Unapply.Module
    lazy val Alternatives: pub.Alternatives.Module

  }

}
