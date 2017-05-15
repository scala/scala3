class Foo @deprecated("foo", "2.11.0") (x: Int)

class Bar @deprecated(x: Int)

class Baz @deprecated()

class C
object obj extends C

class ann(x: C)(y: C, s: String) extends scala.annotation.Annotation

class Bam @ann(obj)(obj, "h")(n: String)

