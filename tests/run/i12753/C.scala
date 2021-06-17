trait C[This <: C[This]]

trait COps[This <: C[This]] {
  def t: This
  def foo(x: Int): This = t
  def bar: Object = "Cbr"
}

class D extends C[D] {
  def x = 1
}
object D extends COps[D] {
  def t = new D
  override def foo(x: Int): D = super.foo(x)
  override def bar: String = "Dbr"
}

trait T {
  val a = 1
  var b = 1
  lazy val c = 1
  def d = 1

  val i: Int
  var j: Int
  lazy val k: Int = 1
  def l: Int
}
object O extends T {
  val i: Int = 1
  var j: Int = 1
  override lazy val k: Int = 1
  def l: Int = 1
}
