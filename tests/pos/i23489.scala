import scala.language.experimental.modularity

class Box1[T <: Singleton](val x: T)
class Box2[T : Singleton](x: => T)
def id(x: Int): x.type = x
def readInt(): Int = ???

def Test = ()
  val x = Box1(id(readInt()))

  val _: Box1[? <: Int] = x

  val y = Box2(id(readInt()))
