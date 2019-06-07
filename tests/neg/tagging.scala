import scala.reflect.ClassTag
object tagging {

  // Tagged[S, T] means that S is tagged with T
  opaque type Tagged[X, Y] = X

  object Tagged {
    def tag[S, T](s: S): Tagged[S, T] = (s: S)
    def untag[S, T](st: Tagged[S, T]): S = st

    def tags[F[_], S, T](fs: F[S]): F[Tagged[S, T]] = fs
    def untags[F[_], S, T](fst: F[Tagged[S, T]]): F[S] = fst

    implicit def taggedClassTag[S, T](implicit ct: ClassTag[S]): ClassTag[Tagged[S, T]] =
      ct
  }

  type @@[S, T] = Tagged[S, T]

  implicit class UntagOps[S, T](st: S @@ T) extends AnyVal {
    def untag: S = Tagged.untag(st)
  }

  implicit class UntagsOps[F[_], S, T](fs: F[S @@ T]) extends AnyVal {
    def untags: F[S] = Tagged.untags(fs)
  }

  implicit class TagOps[S](s: S) extends AnyVal {
    def tag[T]: S @@ T = Tagged.tag(s)
  }

  implicit class TagsOps[F[_], S](fs: F[S]) extends AnyVal {
    def tags[T]: F[S @@ T] = Tagged.tags(fs)
  }

  trait Meter
  trait Foot
  trait Fathom
}
object test {
  import tagging._

  val x: Double @@ Meter = (1e7).tag[Meter]
  val y: Double @@ Foot = (123.0).tag[Foot]
  val xs: Array[Double @@ Meter] = Array(1.0, 2.0, 3.0).tags[Meter]

  val o: Ordering[Double] = implicitly
  val om: Ordering[Double @@ Meter] = o.tags[Meter]
  om.compare(x, x) // 0
  om.compare(x, y) // error
  xs.min(om) // 1.0
  xs.min(o) // error

  // uses ClassTag[Double] via 'Tagged.taggedClassTag'.
  val ys = new Array[Double @@ Foot](20)
}
