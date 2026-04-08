trait ApplicativeError[F[_], E]

object A:
  def generate[F[_]: { ApplicativeError[_[?], ?] }]: Int = 1 // error // error // error // error
