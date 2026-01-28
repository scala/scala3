package scala.quoted.compiletime

/////// Signature ///////////////////////////////////////////////////////////////

/** The signature of a method. */
trait Signature private[compiletime] () {
  /** The signatures of the method parameters.
    *
    *  Each *type parameter section* is represented by a single Int corresponding
    *  to the number of type parameters in the section.
    *  Each *term parameter* is represented by a String corresponding to the fully qualified
    *  name of the parameter type.
    */
  def paramSigs: List[String | Int]
  /** The signature of the result type. */
  def resultSig: String
}
object Signature {

  def quoted(using quotes: Quotes): Signature.Module = quotes.reflectV2.Signature
  given moduleConversion: (quotes: Quotes) => Conversion[Signature.type, Signature.Module] = _ => quotes.reflectV2.Signature

  /** Matches the method signature and returns its parameters and result type. */
  def unapply(sig: Signature): (List[String | Int], String) = (sig.paramSigs, sig.resultSig)

  trait Module private[compiletime] () {}

}
