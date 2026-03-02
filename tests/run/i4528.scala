import scala.reflect.Selectable.reflectiveSelectable

type T = {val a: Int; def a_=(x: Int): Unit}

@main def Test() =
  val x: T = (new { var a = 10 }).asInstanceOf[T]
  assert(x.a == 10)
  x.a = 11
  assert(x.a == 11)
