object A:
  def f: String = ""

trait B:
  def f: String = "abc"

object C extends B:
  export A._            // error

