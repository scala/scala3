import language.experimental.captureChecking
trait IterableOnce[+T]
trait Iterable[+T] extends IterableOnce[T]:
  def flatMap[U, C^](f: T => IterableOnce[U]^{C}): Iterable[U]^{this, C}


class IterableOnceExtensionMethods[T](val it: IterableOnce[T]) extends AnyVal:
  def flatMap[U, C^](f: T => IterableOnce[U]^{C}): IterableOnce[U]^{C} = it match
    case it: Iterable[T] => it.flatMap(f)

