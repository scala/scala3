class Foo @deprecated("foo", "2.11.0") (x: Int)

class Bar @deprecated(x: Int)

class Baz1 @deprecated(implicit c: C)
class Baz2 @deprecated()(implicit c: C)
class Baz3 @deprecated()()(implicit c: C)

object Test {
  implicit val c: C = obj
  new Baz1
  new Baz2
  new Baz3()
}

class D(implicit x: C)

class C
object obj extends C

class ann(x: C)(y: C, s: String) extends scala.annotation.Annotation

class Bam @ann(obj)(obj, "h")(n: String)

// #2515
class Foo2 @deprecated() (@deprecated() id: String)


