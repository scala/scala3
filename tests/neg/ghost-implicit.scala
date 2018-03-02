object Test {

  fun // error

  def fun(implicit a: Double): Int = 42

  ghost implicit def doubleImplicit: Double = 42.0
}
