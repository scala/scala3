abstract class Test {

  var field: Int
  val field2: Int

  def foo() = {

    var x: Int = 1

    def inner() = {
      x = x + 1 + field + field2
    }
  }
}
