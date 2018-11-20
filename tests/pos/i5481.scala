object TypeAlias {

  type Set[A] = A => Boolean

  object Set {
    def singleton[A](a: A): Set[A] = _ == a       // Works
  }
}

object OpaqueType {

  opaque type Set[A] = A => Boolean

  object Set {
    def singleton[A](a: A): Set[A] = _ == a     // Does not compile
    def singleton0[A](a: A): Set[A] = (_: A) == a // Works
  }
}