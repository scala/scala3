trait Comment {
  var comment: String
}

case class Y(val comment: String) extends Comment // error

class Z extends Comment: // error
  val comment: String = ""

class X extends Comment: // error
  override def comment: String = "" // error

class W extends Comment // error


class OK:
  val comment: String = ""
  def comment_=(x: String): Unit = ()

