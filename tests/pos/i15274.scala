enum Format[A]:
  case Str[Next](next: Format[Next]) extends Format[(String, Next)]

def printf[A](format: Format[A], params: A): Unit = (format, params) match
  case (Format.Str(next), (str, rest)) =>
    val s: String = str
