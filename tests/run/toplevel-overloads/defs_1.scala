package top

def hello(name: String) = s"hello, $name"
def hello(x: Int) = x.toString

object O {
  def hi = hello("Bob")
}
