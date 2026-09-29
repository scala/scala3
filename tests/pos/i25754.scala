object Tag {
  opaque type @@[A, T] = A
  inline def tag[T]: [A] => A => A @@ T = [A] => a => a
}

import Tag.*

inline def test[A](f: (Int @@ Unit) => A): A = f(tag[Unit](1))

val _ = test(identity)
