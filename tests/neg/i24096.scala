//> using options -source:future

abstract class Pull[+F[_], +O, +R]
object Pull:
  abstract class Terminal[+R] extends Pull[Nothing, Nothing, R]

  class StreamPullOps[F[_], O](self: Pull[F, O, Unit]):
    def flatMapOutput[F2[x] >: F[x], O2](): Pull[F2, O2, Unit] =
      self match
        case r: Terminal[?] => r // error
        case _              => ???
