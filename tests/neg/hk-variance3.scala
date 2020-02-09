object Test4 {
  def x: Any { type F[+X] } = ???
  def y: Any { type F[X]} = ???
  val z = if ??? then x else y
  val z2 = if ??? then y else x
  val zc: Any { type F[+X] } = z // error
  val z2c: Any { type F[+X] } = z2  // error
}