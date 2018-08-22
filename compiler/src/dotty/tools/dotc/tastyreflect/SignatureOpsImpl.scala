package dotty.tools.dotc.tastyreflect

trait SignatureOpsImpl extends scala.tasty.reflect.SignatureOps with TastyCoreImpl {

  object Signature extends SignatureExtractor {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)] = {
      Some((x.paramsSig.map(_.toString), x.resSig.toString))
    }
  }

  def SignatureDeco(sig: Signature): SignatureAPI = new SignatureAPI {
    def paramSigs: List[String] = sig.paramsSig.map(_.toString)
    def resultSig: String = sig.resSig.toString
  }

}
