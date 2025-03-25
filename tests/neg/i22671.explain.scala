//> using options -explain

object X:
  val w: Int = 42
  def w(y: Int): Int = x + y
  def x: Int = 42
  def x(y: Int): Int = x + y
  val z = 26

trait T:
  def t = 42

def w =
  X.w = 27 // error

def f =
  X.x = 27 // error

def h =
  import X.x as y
  y = 27 // error overload renamed

def i =
  import X.z as y
  y = 27 // error val renamed

def j =
  val x = 42
  x = 27 // error local

def t(t: T) =
  t.t = t // error
