implicit def foo[T <: Tuple.meow]: Unit = ??? // error
given [T <: Tuple.meow]: Unit = ??? // error
