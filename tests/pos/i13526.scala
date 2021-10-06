type ~>[Args <: Tuple, Return] = Args match {
  case (arg1, arg2) => ((arg1, arg2) => Return)
}

trait Builder[Args <: NonEmptyTuple] {
  def apply(f: Args ~> String): String
}

class BuilderImpl[Args <: NonEmptyTuple] extends Builder[Args] {
  override def apply(f: Args ~> String): String = ???
}

val builder = BuilderImpl[Int *: String *: EmptyTuple]()
// builder { (i: Int, s: String) => "test" } // This line compiles
val _ = builder { (i, s) => "test" } // Does not compile