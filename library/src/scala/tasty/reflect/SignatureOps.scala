package scala.tasty.reflect

trait SignatureOps extends Core {

  /** Erased (JVM) signatures. */
  val Signature: SignatureExtractor
  abstract class SignatureExtractor {
    /** Matches the erased (JVM) signature and returns its parameters and result type. */
    def unapply(sig: Signature)(implicit ctx: Context): Option[(List[String], String)]
  }

  trait SignatureAPI {
    /** The (JVM) erased signatures of the parameters. */
    def paramSigs: List[String]
    /** The (JVM) erased result type. */
    def resultSig: String
  }
  implicit def SignatureDeco(sig: Signature): SignatureAPI

}
