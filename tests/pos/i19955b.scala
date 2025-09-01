
trait Wrap[W]

trait IsWrapOfInt[R]:
  type Out <: Int
given [W <: Int, R <: Wrap[W]] => IsWrapOfInt[R]:
  type Out = Int

trait IsInt[U <: Int]
given [U <: Int] => IsInt[U] = ???

extension [L](lhs: L) def get(using ev: IsWrapOfInt[L]): ev.Out = ???
extension (lhs: Int) def isInt(using IsInt[lhs.type]): Unit = ???

val x: Wrap[Int] = ???
val works = (x.get: Int).isInt
val fails = x.get.isInt
