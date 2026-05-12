//> using options -language:experimental.companionScopeInference

// SIP-80 chaining: companion-inferred head followed by method chains.
// Cases from the SIP-134 discussion (Raquo, Lihaoyi).

object Chains:

  // 1. Direct method chain on the inferred companion member's return type.
  //    `Red.withBlueChannel(255)` — Red is Color.Red (Color), then .withBlueChannel
  //    is a method on Color returning Color.
  class Color(val r: Int, val g: Int, val b: Int):
    def withBlueChannel(b: Int): Color = Color(r, g, b)
    def withRedChannel(r: Int): Color = Color(r, g, b)
    def luminosity: Luminosity = Luminosity((r + g + b) / 3)
    def mix(other: Color): Color = Color((r + other.r) / 2, (g + other.g) / 2, (b + other.b) / 2)
  object Color:
    val Red: Color = Color(255, 0, 0)
    val Green: Color = Color(0, 255, 0)
    val Blue: Color = Color(0, 0, 255)
    val Black: Color = Color(0, 0, 0)
    // A sub-namespace exposed through Color's companion. `palette` itself
    // is NOT a Color — only the result of the full chain is.
    val palette: Palette.type = Palette
    object Palette:
      val warm: Warm.type = Warm
      val cool: Cool.type = Cool
      object Warm:
        val sunset: Color = Color(255, 100, 50)
        val ember: Color  = Color(200, 50, 0)
      object Cool:
        val ocean: Color = Color(0, 100, 200)
        val ice: Color   = Color(200, 240, 255)

  class Luminosity(val value: Int):
    def asGrey: Color = Color(value, value, value)
    def doubled: Luminosity = Luminosity(value * 2)

  // Direct method chain returning the target type.
  val purpleish: Color = Red.withBlueChannel(255)

  // Chain through an intermediate unrelated type, ending in the target.
  val grey: Color = Red.luminosity.asGrey

  // Multi-step chain on intermediate.
  val brightGrey: Color = Red.luminosity.doubled.asGrey

  // Companion-inferred head used as method arg.
  val mixed: Color = Red.mix(Blue)

  // Both head and arg inferred from companion in the same call.
  def blend(a: Color, b: Color): Color = a.mix(b)
  val blended: Color = blend(Red, Blue)

  // Companion-inferred head with chained method, all inside a call.
  val mixedChain: Color = blend(Red.withBlueChannel(100), Green.withRedChannel(50))

  // 2. The head of the chain is NOT itself the expected type — only the
  //    full chain is. `palette` resolves via `Color`'s companion (where it
  //    lives as a sub-namespace handle), then `.warm.sunset` walks down to
  //    a `Color`. Companion-scope inference looks at the expected type's
  //    SelectionProto chain and recovers `Color` as the principal target,
  //    so `palette` is found.
  val sunsetColor: Color = palette.warm.sunset
  val ice: Color         = palette.cool.ice

  // Same trick inside a call.
  val vp2: List[Color] = blendList(palette.warm.ember, palette.cool.ocean)
  def blendList(cs: Color*): List[Color] = cs.toList

  // Verify the basic "method chain after companion-inferred head" case works
  // inside varargs and named args too.
  def paletteOf(colors: Color*): List[Color] = colors.toList
  val vp: List[Color] = paletteOf(Red, Green.withBlueChannel(40), Blue.mix(Red))

  def styled(primary: Color = Red, accent: Color = Blue): (Color, Color) = (primary, accent)
  val s1: (Color, Color) = styled(primary = Red.withBlueChannel(20))
  val s2: (Color, Color) = styled(accent = Green.luminosity.asGrey)

end Chains
