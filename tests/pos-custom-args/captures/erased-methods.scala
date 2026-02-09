import language.experimental.saferExceptions
import language.experimental.erasedDefinitions
import language.experimental.captureChecking
import caps.fresh

class Ex1 extends Exception("Ex1")
class Ex2 extends Exception("Ex2")
class Ex3 extends Exception("Ex3")

def foo8a(i: Int) =
  (erased xx1: CanThrow[Ex2]) ?=> throw new Ex2

def foo9a(i: Int)
  : (erased x$0: CanThrow[Ex3])
    ?=> (erased x$1: CanThrow[Ex2])
    ?->{fresh} (erased x$2: CanThrow[Ex1])
    ?->{fresh} Unit
  = (erased x$1: CanThrow[Ex3])
     ?=> (erased x$2: CanThrow[Ex2])
     ?=> (erased x$3: CanThrow[Ex1])
     ?=> throw new Ex3
