//> using options -Yexplicit-nulls
import language.experimental.magic
class C

extension (ctx: C) inline def unapply(input: String): String? = "hi"

@main def run = {
  val O = new C
  val O(x) = "3"
}
