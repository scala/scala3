//> using options -Wconf:cat=deprecation&msg=bruh:warning

class Location @deprecated("No loke", since="1.0") (value: String)
object Location {
  def apply(value: String): Location = new Location(value) // nopos-warn summarized 1 deprecation warning
}

class Brocation @deprecated("bruh!", since="1.0") (value: String)
object Brocation {
  def apply(value: String): Brocation = new Brocation(value) // warn reported deprecated since 1.0: bruh!
}
