class A {
  enum B extends java.lang.Enum[B] { // error: enum extending java.lang.Enum must be declared in a static scope
    case C
  }
}
