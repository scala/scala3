package top

def hello(name: String) = s"hello, $name"
def hello(x: Int) = x.toString

object O {
  def hi = hello("Bob")
  def gb = hello(true)  // OK
}

val test1 = top.hello(false)   // OK, all overloaded variants are considered

val test2 = hello(false)  // error , since we now consider only local overloaded definitions
                          // in the same compilation unit.
                          // See comment on line 280 in Typer#findRef.
