import scala.annotation.experimental
import scala.quoted.*

@experimental class C

def checkExperimental(using Quotes) =
  '{
     println(new C) // error
   }
