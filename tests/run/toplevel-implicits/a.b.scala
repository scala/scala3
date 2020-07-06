package implicits

case class C()

implicit object Cops {
  extension (x: C) def pair(y: C) = (x, y)
}

class D {
  override def toString = "a D"
}

implicit class Ddeco(x: D) {
  def pair(y: D) = (x, y)
}

trait TC[X]

implicit def toC(x: String): C = new C()
implicit def listTC[X: TC]: TC[List[X]] = null
implicit val intTC: TC[Int] = null

