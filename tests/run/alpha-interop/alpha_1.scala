package alpha
import annotation.targetName

abstract class Alpha[T] {

  def foo() = 1

  @targetName("bar") def foo(x: T): T

  @targetName("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

@targetName("Bar") class | extends Alpha[String] {

  @targetName("bar") override def foo(x: String) = x ++ x

  @targetName("append") override def ++ (xs: Alpha[String]) = this

}