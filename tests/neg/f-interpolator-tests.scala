
trait T {
  val s = "um"
  def `um uh` = f"$s%d"  // error

  // "% y" looks like a format conversion because ' ' is a legal flag
  def i11256 =
    val x = 42
    val y = 27
    f"x % y = ${x % y}%d" // error: illegal conversion character 'y'
}
