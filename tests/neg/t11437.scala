
class Regress {
  var v: Int = compiletime.uninitialized
  def f = 42
  var w: Int = (_)    // error: not default value syntax
}
