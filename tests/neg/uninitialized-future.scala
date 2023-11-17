import scala.language.future
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // error
  var b: Int = uninitialized
