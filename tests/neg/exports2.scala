object A with
  def f: String = ""

trait B with
  def f: String = "abc"

object C extends B with
  export A._            // error

