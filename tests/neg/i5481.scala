object TypeAlias {

  type Set[A] = A => Boolean

  object Set {
    def singleton[A](a: A): Set[A] = _ == a
  }
}

object OpaqueType {

  opaque type Set[A] = A => Boolean

  object Set {
    def singleton[A](a: A): Set[A] = _ == a
    def singleton0[A](a: A): Set[A] = (_: A) == a
  }
}
object Test {
  def singleton[A](a: A): Set[A] = _ == a       // error: missing parameter type
}