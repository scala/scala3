object B {
  rewrite def getInline: Int =
    sys.error("This is an expected failure when running C")
}
