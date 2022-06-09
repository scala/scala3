enum Format[A]:
  case Str[Next](next: Format[Next]) extends Format[(String, Next)]
  case Num[Next](next: Format[Next]) extends Format[(Int, Next)]
  case Constant[Next](value: String, next: Format[Next]) extends Format[Next]
  case Done extends Format[Unit]

def printf[A](format: Format[A], params: A): Unit = (format, params) match
  case (Format.Done, ()) =>
    ()

  case (Format.Constant(value, next), params) =>
    println(value)
    printf(next, params)

  case (Format.Str(next), (str, rest)) =>
    println(str)
    printf(next, rest)

  case (Format.Num(next), (i, rest)) =>
    println(i)
    printf(next, rest)
