//> using options -Wconf:cat=deprecation:warning

class Location @deprecated("", "") (value: String)
object Location {
  def apply(value: String): Location = new Location(value) // warn
}
