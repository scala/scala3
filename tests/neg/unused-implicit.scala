object Test {

  fun // error

  def fun(implicit a: Double): Int = 42

  unused implicit def doubleImplicit: Double = 42.0
}
