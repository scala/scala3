
object signatureCollision:
    def f[T](x: T)[U](y: U) = (x,y)
    def f[T](x: T, y: T) = (x,y) // error
