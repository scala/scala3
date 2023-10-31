object Test {
    (Vector(): Seq[?]) match { case List() => true; case Nil => false }
}
