object Test {
  val nil = scala.collection.immutable.Nil
  def f(x: nil.type): Int = 3

  f(scala.collection.immutable.Nil)
}
