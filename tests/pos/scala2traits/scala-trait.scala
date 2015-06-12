// This is supposed to be compiled by Scala 2.11
trait T {
  def d = 42
  val v = ""
  object O
  final val w = 33
}

trait S2T {
  var x: Int = 0
  lazy val y: Int = 1
  val z: Int = 2
  val a: Int
  var b: Int

  def f(x: Int): Int = x + y
}

trait S2Tprivate {
  private var x: Int = 0
  private lazy val y: Int = 1
  private val z: Int = 2

  private def f(x: Int): Int = x + y
  def xx = x
  def xx_=(x: Int) = this.x = x
  def yy = y
  def zz = z
  def ff(x: Int) = f(x)
}

