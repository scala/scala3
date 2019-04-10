object Test {

  fun // error

  def fun(implicit a: Double): Int = 42

  erased implicit def doubleImplicit: Double = 42.0

  def foo erased (implicit a: Double) = 42   // error
}
