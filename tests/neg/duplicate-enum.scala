object Test {
  enum E { // error
    case K0
  }
  enum E: // error
    case K0(v0: Long)
}
