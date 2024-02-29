import scala.language.`future-migration`
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // warn
  var b: Int = uninitialized
