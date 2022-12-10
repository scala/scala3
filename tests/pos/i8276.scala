object NOTHING

inline given [A]: Conversion[NOTHING.type, Option[A]] with
  def apply(x: NOTHING.type) = None

def apply[A](p: Vector[A], o: Option[A] = NOTHING): Unit = ???

def test = apply[String](Vector.empty)