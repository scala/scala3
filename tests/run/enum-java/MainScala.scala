enum A extends java.lang.Enum[A] {
  case MONDAY, TUESDAY, SATURDAY
}

enum B(val gravity: Double) extends java.lang.Enum[B] {
  case EARTH extends B(9.8)
  case JUPITER extends B(100)
  case MOON extends B(4.3)
  case Foo extends B(10)
}
