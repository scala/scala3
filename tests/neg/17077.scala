case class IsIntResult()

object IsInt:
    def unapply(x: Int): IsIntResult = IsIntResult()

@main def test =
  val v: String | Int = "Blop"
  val res =
    v match
      case IsInt() => 43 // error: cannot use a product of arity zero as a return type for unapply
                         // see UnapplyInvalidReturnType in messages.scala
                         // and https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html#fixed-arity-extractors
      case _ => 42
  println(res)
