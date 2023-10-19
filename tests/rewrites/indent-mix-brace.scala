// A mix of nested in-brace regions and indented regions

class A:
  def m1 = {
    ""
  }

  def m2 = {
def m3 =
  val x = ""
  x
m3
  }

class B {
  def foo =
    def bar = {
      ""
    }
    bar
}
