import scala.language.`future-migration`
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // warn: migration warning
  var b: Int = uninitialized
