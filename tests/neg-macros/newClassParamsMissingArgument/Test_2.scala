//> using options -experimental

@main def Test: Unit = {
  makeClass("foo") // error // error
}
