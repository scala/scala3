import scala.quoted.*

def useQuotes(using Quotes) =
  import quotes.reflect.*

  def useFieldMember(s: Symbol) = s.fieldMember("abc") // error
  def getWildcard: Wildcard = ??? // error
  def acceptWildcard(w: Wildcard) = "" // error
  def boundByWildcard[T <: Wildcard]: T = ??? // error

  (useFieldMember, getWildcard, acceptWildcard) // error

def equality =
  import language.strictEquality
  val a = 1 :: Nil == Nil
  val b = Some(5) == None // error