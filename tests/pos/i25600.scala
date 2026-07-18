trait T:
  @annotation.varargs
  def t(s: String, a: Any*): String

val x: T = (s, a) => s
