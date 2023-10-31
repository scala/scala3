// Minimisation of how the fix for t9419 affected zio-http
import java.util.concurrent.Future as JFuture

trait Test:
  def shutdownGracefully(): JFuture[?]

  def executedWildcard(jFuture: => JFuture[?]): Unit
  def executedGeneric[A](jFuture: => JFuture[A]): Unit
  def executedWildGen[A](jFuture: => JFuture[? <: A]): Unit

  // Even though JFuture is morally covariant, at least currently,
  // there's no definition-side variance, so it's treated as invariant.
  // So we have to be concerned that two different values of `JFuture[A]`
  // with different types, blowing up together.  So error in `fails`.
  def works = executedWildcard(shutdownGracefully())
  def fails = executedGeneric(shutdownGracefully()) // error
  def fixed = executedGeneric(shutdownGracefully().asInstanceOf[JFuture[Any]]) // fix
  def best2 = executedWildGen(shutdownGracefully()) // even better, use use-site variance in the method
