package p
import language.experimental.captureChecking

class File

inline def id[T](x: T): T = x

// The type argument `File^{x}` of `id` is inferred. It is read from TASTy where
// `viaInline` is expanded in another compilation unit, which happens before
// capture checking. PostTyper marks it as inferred, so that it is not taken as
// the explicit type `File` there.
transparent inline def viaInline(x: File^): Unit =
  val y: File^{x} = x
  id(y)
  ()
