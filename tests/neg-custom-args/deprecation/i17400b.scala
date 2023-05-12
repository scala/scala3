import scala.quoted.*

class B:
  @deprecated def dep: Int = 1

def checkDeprecated(using Quotes) =
  '{
     val b = new B
     b.dep // error
   }
