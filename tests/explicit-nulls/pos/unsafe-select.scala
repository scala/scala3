class C {
  var x: String = ""
  var y: String | Null = null
  var child: C | Null = null
}

class S {
  import scala.language.unsafeNulls

  val c: C | Null = new C

  def test1 = {
    val x1: String = c.x
    val x2: String | Null = c.x
    val y1: String = c.y
    val y2: String | Null = c.y
    val c1: C = c.child
    val c2: C | Null = c.child

    val yy: String = c.child.child.y
  }

  def test2 = {
    c.x = ""
    c.x = null
    c.y = ""
    c.y = null
    c.child = c
    c.child = null
  }
}