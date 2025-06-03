import scala.language.`3.4`
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // warn
  var b: Int = uninitialized
