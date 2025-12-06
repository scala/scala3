//> using options -Wunused:all

class Foo {
  def foo(): Any = {
    var i = 0 // warn mutated but not read
    val f = () => i += 1
    f
  }
  def bar(): Any = {
    var i = 0 // warn
    val g = () => i = i + 1
    g
  }
  object Select:
    private var i = 0 // warn
  class Select:
    def test = Select.i += 1
}
