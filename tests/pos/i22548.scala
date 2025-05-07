trait Bar[T]
class Foo[T <: Bar[T]] (private val buffer: Any) extends AnyVal
