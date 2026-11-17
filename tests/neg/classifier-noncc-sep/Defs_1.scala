//> using options -experimental
// Compiled without capture checking.
trait Weird extends caps.Classifier

class Buf extends caps.Unscoped, Weird:
  var x = 0
  def put(y: Int): Unit = x = y

