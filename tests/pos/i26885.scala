def UNINITIALIZED[T] = null.asInstanceOf[T]

extension [A](self: A) inline def pipe[B](inline f: (self.type) => B): B = f(self)

def main() =
    summon[deriving.Mirror.Of[(Int, Int)]]
    .pipe: c =>
        UNINITIALIZED[c.MirroredMonoType]
