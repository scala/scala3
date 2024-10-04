object NOTHING

inline given [A] => Conversion[NOTHING.type, Option[A]] = _ => None

def apply[A](p: Vector[A], o: Option[A] = NOTHING): Unit = ???

def test = apply[String](Vector.empty)