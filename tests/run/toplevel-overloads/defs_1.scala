package top

def hello(name: String) = s"hello, $name"
def hello(x: Int) = x.toString

object O {
  def hi = hello("Bob")
  def gb = hello(true)
}

val test1 = top.hello(false)
// val test2 = hello(false)   // does not work anymore, see comment on line 280 in Typer#findRef.
