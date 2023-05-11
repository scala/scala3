class Enclosing:
  object Tags:
    opaque type Ref[T, S <: String & Singleton] = S
    inline def require[T, S <: String & Singleton]: Ref[T, S] = ???
  import Tags.*

  val t1 = require[Int, "t1"]
  val t2 = require[Double, "t2"]
