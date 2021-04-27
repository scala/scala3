class C {
  var x: String = ""
  var y: String | Null = null
  var child: C | Null = null
}

class S {
  val c: C = new C
  val d: C | Null = c

  def test1 = {
    val x1: String = c.x
    val x2: String | Null = c.x
    val y1: String = c.y // error
    val y2: String | Null = c.y
    val c1: C = c.child // error
    val c2: C | Null = c.child

    val yy: String = c.child.child.y // error
  }

  def test2 = {
    c.x = ""
    c.x = null // error
    c.y = ""
    c.y = null
    c.child = c
    c.child = null
  }

  def test3 = {
    d.x = "" // error
    d.y = "" // error
    d.child = c // error
  }
}