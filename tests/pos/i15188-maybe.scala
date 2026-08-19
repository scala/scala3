//> using options -Yexplicit-nulls
import language.experimental.magic
object O

extension [T] (ctx: O.type) inline def unapplySeq(input: T): Seq[T]? = Seq(input)

@main
def Main = {
  val O(x) = 3
  println(s"x: $x")
}
