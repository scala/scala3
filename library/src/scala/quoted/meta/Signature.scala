package scala.quoted.meta

trait Signature private[meta] {

  /**
    * The signatures of the method parameters.
    *
    *  Each *type parameter section* is represented by a single Int corresponding
    *  to the number of type parameters in the section.
    *  Each *term parameter* is represented by a String corresponding to the fully qualified
    *  name of the parameter type.
    */
  def paramSigs: List[String | Int]

  /** The signature of the result type */
  def resultSig: String

}
object Signature {

  def api(using meta: Meta): Meta.SignatureAPI = meta.internal.signature
  given Meta => Conversion[Signature.type, Meta.SignatureAPI] = _.api

  /** Matches the method signature and returns its parameters and result type. */
  def unapply(sig: Signature): (List[String | Int], String) = (sig.paramSigs, sig.resultSig)

}
