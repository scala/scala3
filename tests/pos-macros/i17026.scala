import scala.quoted.*
def macroImpl(using Quotes) =
  '{ def weird[A: Type](using Quotes) = Type.of[A] }
