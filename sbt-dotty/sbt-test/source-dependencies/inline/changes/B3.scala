object B {
  transparent def getInline: Int =
    sys.error("This is an expected failure when running C")
}
