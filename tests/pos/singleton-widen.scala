import scala.compiletime.ops.int._

class Test:

  case class Box[T <: Singleton](v: T)

  def id[T <: Singleton](v: T): v.type = v

  def returnUnion(): 2 | 3 = 3

  def test() =
    val a: Int = 9
    val b = id(a)
    val c = Box(a)
    val d = (a + 2).asInstanceOf[a.type + 2]
    val e = a: a.type
    val f = returnUnion()
    val g = 5

  def test2() =
    val a: 9 = 9
    val b = id(a)
    val c = Box(a)
    val d = (a + 2).asInstanceOf[a.type + 2]
    val e = a: a.type
