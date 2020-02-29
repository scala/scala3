@main def Test =
  assert((0x7FFF_FFFF : Float) == 2.14748365E9f)
  assert((0x7FFF_FFFF : Double) == 2.147483647E9)
