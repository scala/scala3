//> using options -experimental -Yno-experimental

import example.{unrollHelper, unrollLast}

class Bar:
  @unrollHelper
  def foo(s: String, n: Int = 1, @unrollLast b: Boolean = true): String = s + n + b
  // generates: def foo(s: String, n: Int): String = foo(s, n, this.foo$default$3)

@main def Test =
  import scala.reflect.Selectable.reflectiveSelectable
  val bar = new Bar
  val oldBar = bar.asInstanceOf[Any { def foo(s : String, n: Int): String }]

  println(bar.foo("a", 3, false))
  println(oldBar.foo("b", 4))
