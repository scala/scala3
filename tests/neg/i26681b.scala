// https://github.com/scala/scala3/issues/26681
class Box[T](val x: T)

class Owner:
  val w: Int = 8
  val b: Box[w.type] = new Box(w)
  def get: Box[w.type] = new Box(w)
  def set(bb: Box[w.type]): Unit = ()

def unstable: Owner = new Owner

def same[T](a: Box[T], b: Box[T]): Unit = ()

def t1 = same(unstable.b, unstable.b) // error
def t2 = same(unstable.get, unstable.get) // error
def t3 = unstable.set(unstable.get) // error
