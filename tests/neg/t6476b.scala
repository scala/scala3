class C {
  val sa = s"""\""" // error: invalid escape
  val sb = s"""\\"""
  val sc = s"""\ """ // error: invalid escape
  val ra = raw"""\"""
  val rb = raw"""\\"""
  val rc = raw"""\ """
}
