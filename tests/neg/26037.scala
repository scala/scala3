object core:
  opaque type T = String | List[T] // error: illegal cyclic type
  object T:
    def make(s: String): T = s

object api:
  export core.T
  export core.T.*
