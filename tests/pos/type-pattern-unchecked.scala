// scalac: -Werror

def foo =
  Seq(List(1))
    .filter {
      case Seq(x): Seq[Int] @unchecked =>
        true
    }
