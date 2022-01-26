
object Test1:
  val x: Int | String = 1
  val y = x
  val z: Int | String = y

object Test2:
  type Sig = Int | String
  def consistent(x: Sig, y: Sig): Boolean = ???// x == y

  def consistentLists(xs: List[Sig], ys: List[Sig]): Boolean =
       xs.corresponds(ys)(consistent)        // OK
    || xs.corresponds(ys)(consistent(_, _))  // error, found: Any, required: Int | String

object Test3:
  def g[X](x: X | String): Int = ???
  def y: Boolean | String = ???
  g[Boolean](y)
  g(y)
  g[Boolean](identity(y))
  g(identity(y))

object TestSingletonsInUnions:
  def is2Or3(a: 2 | 3) = true

  def testValType() =
    val x: 2 | 3 = 2
    val v = x
    is2Or3(v)

  def testDefReturnType() =
    def f(): 2 | 3 = 2
    val v = f()
    is2Or3(v)

  def testSoftUnionInHardUnion() =
    def isStringOr3(a: String | 3) = true

    def f(x: String): x.type | 3 = 3
    val b: Boolean = true
    val v = f(if b then "a" else "b")
    isStringOr3(v)
