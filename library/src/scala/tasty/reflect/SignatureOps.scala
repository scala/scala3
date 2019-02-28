package scala.tasty.reflect

trait SignatureOps extends Core {

  /** Erased (JVM) signatures. */
  object Signature {
    /** Matches the erased (JVM) signature and returns its parameters and result type. */
    def unapply(sig: Signature)(implicit ctx: Context): Option[(List[String], String)] =
      Some((sig.paramSigs, sig.resultSig))
  }

  implicit class SignatureAPI(sig: Signature) {

    /** The (JVM) erased signatures of the parameters */
    def paramSigs: List[String]= kernel.Signature_paramSigs(sig)

    /** The (JVM) erased result type */
    def resultSig: String = kernel.Signature_resultSig(sig)

  }

}
