package implicits

case class C()

implicit object Cops {
  def (x: C) pair (y: C) = (x, y)
}

class D {
  override def toString = "a D"
}

implicit class Ddeco(x: D) {
  def pair(y: D) = (x, y)
}