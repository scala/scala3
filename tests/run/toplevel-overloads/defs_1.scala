package top

def hello(name: String) = s"hello, $name"
def hello(x: Int) = x.toString
def hello(x: Boolean) = if (x) "yes" else "no"

object O {
  def hi = hello("Bob")
  def gb = hello(true)
}

val test = hello(false)
