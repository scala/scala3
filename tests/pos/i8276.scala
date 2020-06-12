object NOTHING

inline given [A] as Conversion[NOTHING.type, Option[A]] = _ => None

def apply[A](p: Vector[A], o: Option[A] = NOTHING): Unit = ???

apply[String](Vector.empty)
