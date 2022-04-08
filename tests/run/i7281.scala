
trait T {
    case class X[A]()
}

object a extends T
object b extends T

val ax = a.X()
val bx = b.X()

@main def Test = assert(ax != bx)
