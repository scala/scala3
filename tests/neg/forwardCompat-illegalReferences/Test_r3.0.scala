import scala.quoted.*

def useQuotes(using Quotes) =
  import quotes.reflect.*

  def useFieldMember(s: Symbol) = s.fieldMember("abc") // error
  def getWildcard: Wildcard = ??? // error
  def acceptWildcard(w: Wildcard) = "" // error
  def boundByWildcard[T <: Wildcard]: T = ??? // error

  val wildcard = getWildcard // error

  type MyWildcard = Wildcard // error

  type Foo[W <: Wildcard] = Any // error

  type Bar[T] = T match { case Wildcard => Any } // error

  type Baz[T] = T match { case String => Wildcard } // error

  trait Wrapped[T]
  trait WrappedWildcard extends Wrapped[Wildcard] // error
  trait WrappedLikeWildcard[W <: Wildcard] extends Wrapped[W] // error

  class Box(w: Wildcard) // error

  def castToWildcard(x: Any) = x.asInstanceOf[Wildcard] // error // error
