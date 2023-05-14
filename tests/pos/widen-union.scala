
object Test1:
  val x: Int | String = 1
  val y = x
  val z: Int | String = y

object Test2:
  val x: 3 | "a" = 3
  val y = x
  val z: Int | String = y

object Test3:
  type Sig = Int | String
  def consistent(x: Sig, y: Sig): Boolean = ???// x == y

  def consistentLists(xs: List[Sig], ys: List[Sig]): Boolean =
       xs.corresponds(ys)(consistent)        // OK
    || xs.corresponds(ys)(consistent(_, _))  // error, found: Any, required: Int | String

object Test4:

  def g[X](x: X | String): Int = ???
  def y: Boolean | String = ???
  g[Boolean](y)
  g(y)
  g[Boolean](identity(y))
  g(identity(y))


