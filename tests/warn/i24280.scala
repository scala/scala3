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

  def nested(): Any =
    var i = 0 // warn, read of i is in RHS of assign to i
    i =
      var j = 0 // nowarn, j is assigned to and read
      j = i + 1
      j

  def escaped(): Any =
    var i = 0 // warn, read of i is in RHS of assign to i, but should nowarn because assigned to j
    var j = 0 // nowarn, j is assigned to and read
    i =
      j = i + 1
      j
}
