// The nullness type annotations read from class files are interpreted, then dropped
// from the types, even for members that are not nullified, like `toString`.

def test(j: J) =
  val a = j.field
  val b = j.get()
  val c = j.list()
  val d = j.toString
  val x1: Int = a // error
  val x2: Int = b // error
  val x3: Int = c // error
  val x4: Int = d // error
