enum Color:
  case Red, Green, Blue

@main def Test(c: Boolean) =
  // These currently give errors. But maybe we should make the actual
  // enum values carry the `EnumValue` trait, and only strip it from
  // user-defined vals and defs?
  val x: EnumValue = if c then Color.Red else Color.Blue // error // error
  val y: EnumValue = Color.Green        // error


