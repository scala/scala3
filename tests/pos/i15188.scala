object O

extension [T] (ctx: O.type) inline def unapplySeq(input: T): Option[Seq[T]] = Some(Seq(input))

@main
def Main = {
  val O(x) = 3
  println(s"x: $x")
}
