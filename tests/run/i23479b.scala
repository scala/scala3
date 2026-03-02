// scalajs: --skip
trait Ord[A]

object Ord:
  sealed trait Rev[A] extends Ord[A]:
    def reverse: Rev[A] = this

  trait IntOrd extends Ord[Int]

  object Int extends IntOrd with Rev[Int]

@main def Test =
  assert(Ord.Int.getClass.getDeclaredMethods.map(_.getName).toList.contains("reverse"))
