package dotty.tools.tasty.experimental.bridge

import dotty.tools.tasty.experimental.function._

import reflect.ClassTag

trait SignatureOps extends Core with

  given ClassTag[Signature] = internal.Signature_CT

  object Signature with

    type ParamSig = internal.Signature_ParamSig

    def unapply(signature: Signature): (List[ParamSig], TypeName) = internal.Signature_unapply(signature)

    given ParamSigOps: (paramSig: ParamSig) with
      def fold[A](onInt: Int => A, onTypeName: TypeName => A): A = internal.Signature_ParamSig_fold(paramSig)(onInt, onTypeName)
      def foldInt(onInt: IntToInt, onTypeName: ToInt[TypeName]): Int = internal.Signature_ParamSig_foldInt(paramSig)(onInt, onTypeName)

  end Signature

  given SignatureOps: (signature: Signature) with
    def isNotAMethod: Boolean = internal.Signature_isNotAMethod(signature)
