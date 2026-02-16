//> using options -Wunused:all
object Anns {
  final class S extends annotation.StaticAnnotation
}

object Main {
  locally {
    import Anns.*
    class C[@S A]
    C().toString
  }
  locally {
    import Anns.S as T
    class C[@T A]
    C().toString
  }
  locally {
    import scala.specialized as T
    class C[@T A]
    C().toString
  }
  locally {
    import scala.specialized as T // warn
    class C[A]
    C().toString
  }
}
