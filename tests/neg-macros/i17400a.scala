import scala.quoted.*

class A:
  def a: Int = 1

def checkOverride(using Quotes) =
  '{
     class Sub extends A:
       def a: String = "" // error
     new Sub
   }
