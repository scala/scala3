package bugs.shadowing

object x {
  def f421() = 1
}

object y {
  def f421() = true
}

import x.f421
import y.f421

object test {
  println(f421()) // error
}