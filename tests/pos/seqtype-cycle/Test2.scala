package object scala {
  type Throwable = java.lang.Throwable // needed for some reasons

  type Seq[+A] = scala.Seq[A]
}
