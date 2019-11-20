package scala.tasty.reflect

trait SignatureOps extends Core {

  /** The signature of a method */
  object Signature {
    /** Matches the method signature and returns its parameters and result type. */
    def unapply(sig: Signature)(given ctx: Context): Option[(List[String | Int], String)] =
      Some((sig.paramSigs, sig.resultSig))
  }

  given SignatureOps: (sig: Signature) {

    /** The signatures of the method parameters.
      *
      *  Each *type parameter section* is represented by a single Int corresponding
      *  to the number of type parameters in the section.
      *  Each *term parameter* is represented by a String corresponding to the fully qualified
      *  name of the parameter type.
      */
    def paramSigs: List[String | Int] = internal.Signature_paramSigs(sig)

    /** The signature of the result type */
    def resultSig: String = internal.Signature_resultSig(sig)

  }

}
