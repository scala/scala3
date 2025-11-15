//> using options -Werror -deprecation -feature

// check that deprecation warnings of Red are not caught in its enclosing scope
enum Color(rgb: Int) {

  @deprecated("stop using Red", "0.1")
  case Red extends Color(0xff0000)

  case Green extends Color(0x00ff00)

  case Blue  extends Color(0x0000ff)

  final def colorCode: Option[Int] = this match {
    case Red => None
    case _   => Some(rgb)
  }

}

object Color {
  val deprecatedMembers = Set(Red)
}
