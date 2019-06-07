class C[T]
object C { def derived[T]: C[T] = ??? }

object X extends C[X.type] derives C
