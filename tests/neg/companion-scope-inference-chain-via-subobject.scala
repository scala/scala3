//> using options -language:experimental.companionScopeInference

// SIP-80 boundary: companion lookup uses the expected type's OWN companion,
// not the companion of a sibling object whose chained call produces the
// expected type. This case (raised by @raquo / @lihaoyi in SIP-134) requires
// a much broader search and is out of scope for v1.
object SubObjectChain:

  class Color(val name: String)

  // `warm` lives in a sibling object (Palette), not in `Color`'s companion.
  object Palette:
    val warm: Warm.type = Warm
    object Warm:
      val sunset: Color = Color("sunset")

  def paint(c: Color): String = c.name

  // The chain `warm.sunset` would produce a `Color`, but companion-scope
  // inference only searches `Color`'s companion for `warm` — it isn't
  // there, so this is rejected.
  val s: String = paint(warm.sunset) // error

end SubObjectChain
