package dotty.tools.scaladoc.cc

import scala.quoted._

object CaptureDefs:
  /** The name of the `retains` annotation. */
  val RetainsName: String = "scala.annotation.retains"

  /** The name of the `retainsCap` annotation. */
  val RetainsCapName: String = "scala.annotation.retainsCap"

  /** The name of the `retainsByName` annotation. */
  val RetainsByNameName: String = "scala.annotation.retainsByName"

  def retains(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass(RetainsName)
  def retainsCap(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass(RetainsCapName)
  def retainsByName(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass(RetainsByNameName)
end CaptureDefs

extension (using qctx: Quotes)(term: qctx.reflect.Term)

  /** Is this term a `retains`* annotations from capturing types?  */
  def isRetains: Boolean =
    val sym = term.tpe match
      case qctx.reflect.AppliedType(base, _) => base.typeSymbol
      case other                             => other.typeSymbol
    sym == CaptureDefs.retains
    || sym == CaptureDefs.retainsCap
    || sym == CaptureDefs.retainsByName