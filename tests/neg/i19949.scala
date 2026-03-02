
trait T[N]:
  type M = N match
    case 0 => Any

val t: T[Double] = new T[Double] {}
val x: t.M = "hello" // error

val z: T[Double]#M = "hello" // error
