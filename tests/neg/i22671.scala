object X:
  val w: Int = 42
  def w(y: Int): Int = x + y
  def x: Int = 42
  def x(y: Int): Int = x + y
  val z = 26

def w =
  X.w = 27 // error

def f =
  X.x = 27 // error

def g =
  import X.x
  x = 27 // error

def h =
  import X.x as y
  y = 27 // error

def i =
  import X.z as y
  y = 27 // error

def j =
  val x = 42
  x = 27 // error

def k =
  X.x += 27 // error
  1 += 1 // error


object t8763:
  import collection.mutable
  def bar(): Unit =
    val names_times = mutable.Map.empty[String, mutable.Set[Long]]
    val line = ""
    val Array(fields) = line.split("\t")
    names_times(fields(0)) += fields(1).toLong // error

object t9834:
  object x { def apply() = 42 ; def update(i: Int) = () }
  x() += "42" // error

class C(c: Int):
  def test(): Unit =
    c = 42 // error
