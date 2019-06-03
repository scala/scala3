package alpha
import annotation.alpha

abstract class Alpha[T] {

  def foo() = 1

  @alpha("bar") def foo(x: T): T

  @alpha("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

@alpha("Bar") class | extends Alpha[String] {

  @alpha("bar") override def foo(x: String) = x ++ x

  @alpha("append") override def ++ (xs: Alpha[String]) = this

}