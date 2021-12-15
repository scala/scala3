import deriving.MirrorOf

sealed trait A

object A {
  def f() = {
    println(new A {})
    println(new A {})
  }
}

def aMirror = summon[Mirror.Of[A]] // error: cannot take shape, it has anonymous or inaccessible subclasses

sealed trait B

class D(x: Int, y: String) extends B

def bMirror = summon[Mirror.Of[B]] // error: cannot take shape, its subclass class D is not a case class

class E
def eMirror = summon[Mirror.Of[E]] // error: cannot take shape, it is neither sealed nor a case class

sealed trait F
def fMirror = summon[Mirror.Of[F]] // error: cannot take shape, it has anonymous or inaccessible subclasses

object G {
  def f() = {
    case class H() extends F
  }
}

case class I(x: Int, y: String)
object I:
  def f = summon[deriving.Mirror.ProductOf[I]].fromProductTyped((1, 2)) // error
  def g = summon[deriving.Mirror.ProductOf[I]].fromTuple((1, 2)) // error

