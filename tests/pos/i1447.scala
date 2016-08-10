case object X

object Test {
  val Alias = X

  val x: X.type = Alias

  type Alias = X.type
  val a: Alias = Alias
}
