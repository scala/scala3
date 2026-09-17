//> using options -Yexplicit-nulls
import language.experimental.errorHandling
def Test =
  val s: String? = "a"
  s.get  // error
