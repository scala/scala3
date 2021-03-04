type Conv[T] = [X] =>> X => T

trait SemiGroup[T]:
  extension [U: Conv[T]](x: U)
    def combine(y: T): T
  extension (x: T)
    def combine[U: Conv[T]](y: U): T

trait Q[T, R: SemiGroup] extends SemiGroup[T]:
  def res(x: R, y: R) = x.combine(y)
