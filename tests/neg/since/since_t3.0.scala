import scala.quoted.*

def useQuotes(using Quotes) =
  import quotes.reflect.*
  def useFieldMember(s: Symbol) = s.fieldMember("abc") // error
  useFieldMember