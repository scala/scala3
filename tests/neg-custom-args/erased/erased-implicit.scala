object Test {

  fun // error

  def fun(implicit a: Double): Int = 42

  erased implicit def doubleImplicit: Double

}
