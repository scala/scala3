import language.experimental.saferExceptions
import language.experimental.erasedDefinitions
import language.experimental.captureChecking

class Ex1 extends Exception("Ex1")
class Ex2 extends Exception("Ex2")
class Ex3 extends Exception("Ex3")

erased class CT[-E <: Exception] extends caps.Capability

def Throw[Ex <: Exception](ex: Ex)(using CT[Ex]^): Nothing = ???

def foo8a(i: Int) =
 (erased xx1: CT[Ex2]^) ?=> Throw(new Ex2)

def foo9a(i: Int)
  : (x$1: CT[Ex3]^)
    ?=> (x$2: CT[Ex2]^)
    ?-> {x$1, caps.cap} Unit
  = (x$1: CT[Ex3]^)
     ?=> (x$2: CT[Ex2]^)
     ?=>
      //given (CT[Ex3]^) = x$1
      Throw(new Ex3)

def foo10a(i: Int)
  : (erased x$0: CT[Ex3]^)
    ?=> (erased x$1: CT[Ex2]^)
    ?-> {x$0, caps.cap} (erased x$2: CT[Ex1]^)
    ?-> {x$0, x$1, caps.cap} Unit
  = (erased x$1: CT[Ex3]^)
     ?=> (erased x$2: CT[Ex2]^)
     ?=> (erased x$3: CT[Ex1]^)
     ?=> Throw(new Ex3)
