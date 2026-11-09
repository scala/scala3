//> using options -Werror

trait ApplicativeError[F[_], E]

object A:
  def generate[F[_]: { ApplicativeError[_[?], ?] }]: Int = 1 // error // error // error // nopos-error

/* Previous spurious error
4 |  def generate[F[_]: { ApplicativeError[_[?], ?] }]: Int = 1
  |                                            ^
  |                 Illegal context bound: Any does not take type parameters.
*/
