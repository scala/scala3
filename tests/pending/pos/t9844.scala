trait X[T <: X[T]] { self: T => }
object Y extends X[Y.type]
