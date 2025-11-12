trait Ord[X]:
  def compare(x: X, y: X): Int
  type T

trait Show[X]:
  def show(x: X): String

val f0: Show[String] ?=> String = summon[Show[String]].show("hello, world") * 2

val f1: (x: Show[String]) ?=> String = x.show("hello, world") * 2 // error

val f2 = (x: Show[String]) ?=> x.show("hello, world") * 2
