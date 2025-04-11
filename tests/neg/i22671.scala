object X:
  def x: Int = 42
  def x(y: Int): Int = x + y

def test1 =
  X.x = 27 // error

def test2 =
  import X.x
  x = 27 // error

def test3 =
  val x = 42
  x = 27 // error