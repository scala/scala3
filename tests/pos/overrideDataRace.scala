package test

trait Traversable {
  def mkString: String = ???
}

trait ViewMkString {
  self: Traversable =>

  def mkString: String = mkString("")
  def mkString(s: String) = ???

}
