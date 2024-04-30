implicit inline def LiteralStr(s: String)(implicit ctx: P[Any]): P[Unit] = ???

extension [T](inline parse0: P[T]) {
  inline def ~[V, R](inline other: P[V])(using
      ctx: P[?]
  ): P[R] = ${ MacroInlineImpls.parsedSequence0[T, V, R]('parse0, 'other) }

  inline def flatMapX[V](inline f: T => P[V]): P[V] =
    MacroInlineImpls.flatMapXInline[T, V](parse0)(f)
}

def deeper[$: P]: P[Int] = ???
def newline[$: P]: P[Unit] = ???
def blockBody[p: P]: P[Seq[Int]] = newline ~ deeper.flatMapX { i =>
  val y = LiteralStr("")(using ???)
  ???
}
