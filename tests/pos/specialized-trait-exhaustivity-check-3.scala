//> using options -language:experimental.specializedTraits -Werror
import scala.annotation.nowarn

@nowarn("id=E234")
sealed inline trait List[+T: Specialized]
sealed inline trait Nil[T: Specialized] extends List[T]
sealed inline trait :+:[T: Specialized](h: T, t: List[T]) extends List[T]

val xs: List[Double] = new Nil[Double]() {}

def foo(x: List[Double]): Unit = x match {
  case xs: :+:[_] => println("C")
  case _: Nil[_] => 
}
