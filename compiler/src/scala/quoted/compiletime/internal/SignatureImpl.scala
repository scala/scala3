package scala.quoted.compiletime.internal

import dotty.tools.dotc
import scala.quoted.compiletime as pub

/////// Signature ///////////////////////////////////////////////////////////////

type Signature = SignatureImpl
final class SignatureImpl(val underlying: dotc.core.Signature) extends pub.Signature {
  override def paramSigs: List[String | Int] = ???
  override def resultSig: String = ???
}
object SignatureImpl {
  object Module extends pub.Signature.Module {}
}