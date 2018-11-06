package scala.tasty.reflect

trait SignatureOps extends ReflectionCore {

  val Signature: SignatureExtractor
  abstract class SignatureExtractor {
    def unapply(sig: Signature)(implicit ctx: Context): Option[(List[String], String)]
  }

  trait SignatureAPI {
    def paramSigs: List[String]
    def resultSig: String
  }
  implicit def SignatureDeco(sig: Signature): SignatureAPI

}
