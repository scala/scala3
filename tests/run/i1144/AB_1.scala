trait A {
  def x = 3
}
trait B extends A {
  override def x = super.x * 2
}
