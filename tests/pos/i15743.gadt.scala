abstract class C[Z] { def getZ: Z }
final case class C1() extends C[Tuple] { def getZ = new Tuple1[String]("foo") }

// Like pos/i15743 but definitely requires the constraint on T to be stored as a GADT constraint
// where in pos/i15743 it may have been reasonable to think that the constraint could be stored
// in the regular type inference constraints
class Alt:
  def test[T](e: C[T]) = e match
    case c1 @ C1() => // GADT constr: T := Tuple
      val t1: T        = c1.getZ
      val t2: Int *: T = (1: Int) *: t1
      val i1: Int      = (t2: Int *: T).head[Int *: T]
