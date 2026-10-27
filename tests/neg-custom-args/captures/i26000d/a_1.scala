class File
class Box[A](val value: A)

// Capture sets in parameter types of parameters, and lower bounds, are seen
// by other compilation units as written
object Lib:
  def mk(f: File^) = Box((k: File^{f} -> File) => k(f))
  def mk2(f: File^) = Box([C^ >: {f}] => (k: File^{C} -> File) => k(f))
