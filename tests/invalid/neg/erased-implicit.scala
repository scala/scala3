//> using options -language:experimental.erasedDefinitions

object Test {

  fun // error

  def fun(implicit a: Double): Int = 42

  erased implicit def doubleImplicit: Double = 42.0

}
