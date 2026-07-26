//> using options -Ycheck:all

class ann(xs: Any) extends annotation.StaticAnnotation

class Ops1(s: String) extends AnyVal:
  def m: Int @ann(this) = 1
