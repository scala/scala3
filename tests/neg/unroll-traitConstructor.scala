//> using options -experimental

import scala.annotation.unroll

trait Unroll(a: String, @unroll b: Boolean = true): // error
  def show: String = a + b

class Bar(arg: String, bool: Boolean) extends Unroll(arg, bool)
