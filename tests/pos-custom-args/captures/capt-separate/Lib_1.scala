object Lib:
  extension [A](xs: Seq[A])
    def mapp[B](f: A => B): Seq[B] =
      xs.map(f.asInstanceOf[A -> B])


