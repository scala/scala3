class A(v0: Int = 0) {
  override def toString(): String = s"{$v0}"
}

trait B {
  def sayHi(): String = s"Hi from B"
}
