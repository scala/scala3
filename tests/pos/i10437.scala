import scala.language.implicitConversions

final class MyClass(name: String) {

  final class Fun0(val f: Function0[Any])

  object Fun0 {

    implicit def function0AsFun0(f: Function0[Any]): Fun0 = new Fun0(f)

  }

  def apply(f: => Unit): Unit = {
    apply(() => f)
  }

  def apply(fun: Fun0): Unit = {
    // Do something
    println(s"Got a Fun0 $fun")
  }

  def apply[T1](f: (T1) => Any)(implicit m1: Manifest[T1]): Unit = {
    // Do something
    println(s"Got a Function1: ${f}")
  }

}