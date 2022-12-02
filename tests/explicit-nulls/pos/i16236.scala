// Copy of tests/pos/i16236.scala
trait A

def consume[T](t: T): Unit = ()

def fails(p: (Double & A) | Null): Unit = consume(p) // was: assertion failed: <notype> & A

def switchedOrder(p: (A & Double) | Null): Unit = consume(p) // ok
def nonPrimitive(p: (String & A) | Null): Unit = consume(p) // ok
def notNull(p: (Double & A)): Unit = consume(p) // ok
