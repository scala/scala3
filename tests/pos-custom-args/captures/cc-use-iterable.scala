import language.experimental.captureChecking
trait IterableOnce[+T]
trait Iterable[+T] extends IterableOnce[T]:
  def flatMap[U](@caps.use f: T => IterableOnce[U]^): Iterable[U]^{this, f*}


class IterableOnceExtensionMethods[T](val it: IterableOnce[T]) extends AnyVal:
  def flatMap[U](@caps.use f: T => IterableOnce[U]^): IterableOnce[U]^{f*} = it match
    case it: Iterable[T] => it.flatMap(f)

