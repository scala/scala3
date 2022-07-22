package covtest

import scala.compiletime.uninitialized
class Foo:
  var x: AnyRef = uninitialized
