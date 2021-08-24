class Bar(i: Int):
  transparent inline def this() = this(0) // error

val bar = Bar()
