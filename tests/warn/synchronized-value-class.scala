def lit =
  123.synchronized {} // warn

def lit2 =
  true.synchronized {} // warn

def indirect =
  val x = 456L
  x.synchronized {} // warn

def boxedInt(x: java.lang.Integer) =
  x.synchronized {} // warn

@jdk.internal.ValueBased
class MyVal(x: Int)
def valueBased =
  MyVal(456).synchronized {} // warn

def localDate =
  val d = java.time.LocalDate.now()
  d.synchronized {} // warn

def boxedUnit(u: scala.runtime.BoxedUnit) =
  u.synchronized {} // warn
